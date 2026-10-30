open Bactrian_core

type t = {
  kind : Experiment.sandbox_runtime;
  name : string;
  exe : string;
  client_env : string array;
  arch : string;
}

let kind t = t.kind
let name t = t.name
let exe t = t.exe
let client_env t = t.client_env
let arch t = t.arch
let ( let* ) = Result.bind

let client_variables : Experiment.sandbox_runtime -> string list =
  let common =
    [
      "PATH";
      "HOME";
      "USER";
      "LOGNAME";
      "TMPDIR";
      "XDG_RUNTIME_DIR";
      "XDG_CONFIG_HOME";
    ]
  in
  function
  | `Docker ->
      common
      @ [
          "DOCKER_HOST";
          "DOCKER_CONTEXT";
          "DOCKER_CONFIG";
          "DOCKER_CERT_PATH";
          "DOCKER_TLS_VERIFY";
          "DOCKER_API_VERSION";
        ]
  | `Container -> common

let current_client_env kind =
  Array.of_list
    (List.filter_map
       (fun k -> Option.map (fun v -> k ^ "=" ^ v) (Sys.getenv_opt k))
       (client_variables kind))

let last_line text =
  String.split_on_char '\n' text
  |> List.filter (fun l -> not (String.equal (String.trim l) ""))
  |> List.fold_left (fun _ l -> Some (String.trim l)) None

(* Runs [argv] and gives its standard output, or why it failed: the exit
   status and the last line of standard error. *)
let capture ~process_mgr ~clock ~env ~timeout argv =
  let out = Buffer.create 256 and err = Buffer.create 256 in
  match
    Eio.Time.with_timeout clock timeout (fun () ->
        Eio.Switch.run @@ fun sw ->
        Ok
          (Eio.Process.await
             (Eio.Process.spawn ~sw process_mgr argv ~env
                ~stdout:(Eio.Flow.buffer_sink out)
                ~stderr:(Eio.Flow.buffer_sink err))))
  with
  | exception (Eio.Io _ as e) -> Error (Printexc.to_string e)
  | Error `Timeout ->
      Error (Printf.sprintf "it did not finish within %.0f seconds" timeout)
  | Ok (`Exited 0) -> Ok (Buffer.contents out)
  | Ok status ->
      Error
        (Printf.sprintf "%s%s"
           (Proc_util.describe_exit status)
           (match last_line (Buffer.contents err) with
           | Some l -> ": " ^ l
           | None -> ""))

let run_client ~process_mgr ~clock ?(timeout = 30.) t args =
  capture ~process_mgr ~clock ~env:t.client_env ~timeout (t.exe :: args)

let runtime_name : Experiment.sandbox_runtime -> string = function
  | `Docker -> "docker"
  | `Container -> "container"

let find name =
  Proc_util.find_executable ~path:(Sys.getenv_opt "PATH") ~cwd:(Sys.getcwd ())
    name

let detect_docker ~process_mgr ~clock =
  let name = runtime_name `Docker in
  match find name with
  | None ->
      Error
        (Printf.sprintf
           "%s is not on PATH. Fix: install a Docker-compatible runtime \
            (OrbStack or Docker Desktop on macOS, Docker Engine on Linux, \
            Podman is not supported yet)"
           name)
  | Some exe -> (
      let client_env = current_client_env `Docker in
      match
        capture ~process_mgr ~clock ~env:client_env ~timeout:30.
          [ exe; "version"; "--format"; "{{.Server.Arch}}" ]
      with
      | Error why ->
          Error
            (Printf.sprintf
               "%s cannot reach its daemon (%s). Fix: start the runtime \
                (OrbStack, Docker Desktop or the Docker daemon)"
               exe why)
      | Ok text -> (
          match String.trim text with
          | "" -> Error (exe ^ " version did not name the daemon's architecture")
          | arch -> Ok { kind = `Docker; name; exe; client_env; arch }))

let detect_container ~process_mgr ~clock =
  let name = runtime_name `Container in
  match find name with
  | None ->
      Error
        (Printf.sprintf
           "%s is not on PATH. Fix: install Apple's container (macOS 26 on \
            Apple silicon, github.com/apple/container)"
           name)
  | Some exe -> (
      let client_env = current_client_env `Container in
      let fix = Printf.sprintf ". Fix: %s system start" exe in
      match
        capture ~process_mgr ~clock ~env:client_env ~timeout:30.
          [ exe; "system"; "status"; "--format"; "json" ]
      with
      | Error why ->
          Error (Printf.sprintf "%s system status failed (%s)%s" exe why fix)
      | Ok text -> (
          match Container_cli.status text with
          | Error m ->
              Error
                (Printf.sprintf
                   "%s system status gave JSON that is not read: %s" exe m)
          | Ok { running = false; state; _ } ->
              Error
                (Printf.sprintf "%s's services are %S, not running%s" exe state
                   fix)
          | Ok { running = true; architecture = ""; _ } ->
              Error (exe ^ " system status did not name the architecture")
          | Ok { running = true; architecture = arch; _ } ->
              Ok { kind = `Container; name; exe; client_env; arch }))

(* Unset, the runtime is docker when it answers, otherwise container. *)
let detect ~process_mgr ~clock = function
  | Some `Docker -> detect_docker ~process_mgr ~clock
  | Some `Container -> detect_container ~process_mgr ~clock
  | None -> (
      match detect_docker ~process_mgr ~clock with
      | Ok t -> Ok t
      | Error docker -> (
          match detect_container ~process_mgr ~clock with
          | Ok t -> Ok t
          | Error container ->
              Error
                (Printf.sprintf
                   "no container runtime works. docker: %s. container: %s"
                   docker container)))

let image_id ~process_mgr ~clock t image =
  let missing () =
    match t.kind with
    | `Docker ->
        Printf.sprintf "the image %s is not present locally. Fix: %s pull %s"
          image t.name image
    | `Container ->
        Printf.sprintf
          "the image %s is not present locally. Fix: %s image pull %s" image
          t.name image
  in
  let digest out =
    match String.trim out with
    | d when String.starts_with ~prefix:"sha256:" d -> Ok d
    | d ->
        Error
          (Printf.sprintf "%s image inspect gave %S, not an image digest" t.name
             d)
  in
  match t.kind with
  | `Docker -> (
      match
        run_client ~process_mgr ~clock t
          [ "image"; "inspect"; "--format"; "{{.Id}}"; "--"; image ]
      with
      | Ok out -> digest out
      | Error _ -> Error (missing ()))
  | `Container -> (
      match
        run_client ~process_mgr ~clock t [ "image"; "inspect"; "--"; image ]
      with
      | Error _ -> Error (missing ())
      | Ok out -> (
          match Container_cli.image_digest out with
          | Ok d -> digest d
          | Error m ->
              Error
                (Printf.sprintf
                   "%s image inspect gave JSON that is not read: %s" t.name m)))

let create ~process_mgr ~clock t argv =
  match capture ~process_mgr ~clock ~env:t.client_env ~timeout:120. argv with
  | Ok (_ : string) -> Ok ()
  | Error why -> Error (Printf.sprintf "%s create failed (%s)" t.name why)

let created_image ~process_mgr ~clock t name =
  match run_client ~process_mgr ~clock t [ "inspect"; "--"; name ] with
  | Error why -> Error (Printf.sprintf "%s inspect failed (%s)" t.name why)
  | Ok out ->
      Result.map_error
        (fun m ->
          Printf.sprintf "%s inspect gave JSON that is not read: %s" t.name m)
        (Container_cli.container_digest out)

let relay_cache_dir () =
  let base =
    match Sys.getenv_opt "XDG_CACHE_HOME" with
    | Some d when not (Filename.is_relative d) -> d
    | Some _ | None ->
        Filename.concat
          (Option.value (Sys.getenv_opt "HOME") ~default:"/")
          ".cache"
  in
  Filename.concat (Filename.concat base "bactrian") "relay"

let relay_name t =
  Printf.sprintf "relay-%s-%s"
    (String.sub
       Digest.BLAKE256.(
         to_hex
           (string
              (String.concat "\000"
                 (List.concat_map (fun (n, t) -> [ n; t ]) Relay_source.files))))
       0 16)
    (String.map
       (fun c ->
         match c with
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> c
         | _ -> '-')
       t.arch)

(* Alpine 3.24.2 (2026-09-18), the multi-architecture index. *)
let build_image =
  "alpine:3.24.2@sha256:294b683cb724975bec92580e1e685676bd4b50bda910ddb8c51d4cabeaec77e6"

let build_packages = [ "gcc"; "musl-dev" ]
let build_timeout = 600.

let rec mkdirs path =
  if not (Sys.file_exists path) then begin
    mkdirs (Filename.dirname path);
    try Unix.mkdir path 0o700 with Unix.Unix_error (EEXIST, _, _) -> ()
  end

(* Built into a directory of its own and renamed into place, so two bactrian
   processes building at once never see half a file. *)
let build ~process_mgr ~clock t ~dir ~target =
  let work =
    Filename.concat dir ("build-" ^ Proc_util.hex (Mirage_crypto_rng.generate 6))
  in
  let src = Filename.concat work "src" and out = Filename.concat work "out" in
  Fun.protect ~finally:(fun () ->
      try Proc_util.remove_tree work
      with Unix.Unix_error _ | Sys_error _ -> ())
  @@ fun () ->
  List.iter (fun d -> Unix.mkdir d 0o755) [ work; src; out ];
  List.iter
    (fun (name, text) ->
      Out_channel.with_open_bin (Filename.concat src name) (fun oc ->
          output_string oc text))
    Relay_source.files;
  let script =
    Printf.sprintf
      "apk add --no-cache %s >&2 && gcc -static -O2 -Wall -Wextra -Werror -o \
       /out/relay /src/relay.c /src/relay_image.c && chown %d:%d /out/relay"
      (String.concat " " build_packages)
      (Unix.getuid ()) (Unix.getgid ())
  in
  let run =
    match t.kind with
    | `Docker -> [ "run"; "--rm" ]
    | `Container -> [ "run"; "--rm"; "--progress"; "none" ]
  in
  let* _ =
    Result.map_error
      (fun why ->
        Printf.sprintf
          "the relay could not be built in %s (%s). It needs the network once, \
           for Alpine's packages"
          build_image why)
      (run_client ~process_mgr ~clock ~timeout:build_timeout t
         (run
         @ [
             "--mount";
             Printf.sprintf "type=bind,source=%s,target=/src,readonly" src;
             "--mount";
             Printf.sprintf "type=bind,source=%s,target=/out" out;
             build_image;
             "sh";
             "-c";
             script;
           ]))
  in
  Unix.rename (Filename.concat out "relay") target;
  Ok target

(* The relay becomes the container's process 1, so the cache is trusted only as
   far as this user's own files are: [path] must be what [kind] says (not a
   symbolic link), owned by this user and writable by no one else. Any process
   of the user could replace the bactrian executable itself, so a hash of the
   binary would add nothing. *)
let owned ~kind ~what path =
  match Unix.lstat path with
  | exception Unix.Unix_error (ENOENT, _, _) -> Ok false
  | st ->
      if st.st_kind <> kind then
        Error (Printf.sprintf "%s is not %s (a symbolic link?)" path what)
      else if st.st_uid <> Unix.getuid () then
        Error
          (Printf.sprintf "%s belongs to user %d, not to this user" path
             st.st_uid)
      else if st.st_perm land 0o022 <> 0 then
        Error
          (Printf.sprintf "%s is writable by its group or by others (mode %o)"
             path st.st_perm)
      else Ok true

let relay ~process_mgr ~clock t =
  let dir = relay_cache_dir () in
  let target = Filename.concat dir (relay_name t) in
  let untrusted why =
    Error
      (Printf.sprintf "the cached relay cannot be trusted: %s. Fix: remove %s"
         why dir)
  in
  match
    mkdirs dir;
    match owned ~kind:S_DIR ~what:"a directory" dir with
    | Error why -> untrusted why
    | Ok (_ : bool) -> (
        match owned ~kind:S_REG ~what:"a regular file" target with
        | Error why -> untrusted why
        | Ok true -> Ok target
        | Ok false -> (
            let* built = build ~process_mgr ~clock t ~dir ~target in
            match owned ~kind:S_REG ~what:"a regular file" built with
            | Ok true -> Ok built
            | Ok false -> untrusted (built ^ " is missing after the build")
            | Error why -> untrusted why))
  with
  | result -> result
  | exception ((Unix.Unix_error _ | Sys_error _) as e) ->
      Error
        (Printf.sprintf "the relay could not be built in %s: %s" dir
           (Printexc.to_string e))

(* libfaketime *)

(* Version 0.9.13, published 2026-08-31. The archive's checksum is of the
   tarball GitHub serves for the tag (github.com and codeload give the same
   bytes), checked in the container that downloads it and again before each
   build. *)
let faketime_url =
  "https://github.com/wolfcw/libfaketime/archive/refs/tags/v0.9.13.tar.gz"

let faketime_sha256 =
  "8e56deeb805682b025107e095f1d94a6ea677472f05824cd475f5c5e6e1a5ddf"

let faketime_archive = "libfaketime-0.9.13.tar.gz"

(* Debian bookworm of 2026-10-05, pushed 2026-10-06 (glibc 2.36). The library
   needs only GLIBC_2.34 ({!Vm_exec_probe.glibc_floor}), which the relay checks
   in the image before it is preloaded. The multi-architecture index. *)
let glibc_build_image =
  "debian:bookworm-20261005-slim@sha256:7c7b2c966bc9ee8cedfeef67e0e279108992c77681fa595db4a9d65c06ccc587"

(* The archive is handed to the user, as the relay and the libraries are: left
   to root, which a rootful Docker on Linux makes it, it would fail [owned] at
   the next build and keep every run on the other C library contract-only. *)
let faketime_fetch_script =
  Printf.sprintf
    "set -eu; wget -q -O /out/%s %s && echo '%s  /out/%s' | sha256sum -c >&2 \
     && chown %d:%d /out/%s"
    faketime_archive faketime_url faketime_sha256 faketime_archive
    (Unix.getuid ()) (Unix.getgid ()) faketime_archive

(* The same steps in each builder: check the archive, build only the
   multi-thread library (the other targets do not build with musl), run
   [check] on it, and hand it to the user. *)
let faketime_build_script ~install ~check =
  Printf.sprintf
    "set -eu; %s; echo '%s  /src/%s' | sha256sum -c >&2 && mkdir /tmp/lf && \
     tar xzf /src/%s -C /tmp/lf --strip-components=1 && make -C /tmp/lf/src \
     libfaketimeMT.so.1 >&2 && %s && cp /tmp/lf/src/libfaketimeMT.so.1 /out/ \
     && chown %d:%d /out/libfaketimeMT.so.1"
    install faketime_sha256 faketime_archive faketime_archive check
    (Unix.getuid ()) (Unix.getgid ())

(* The glibc library asks for no version of glibc newer than
   {!Vm_exec_probe.glibc_floor}, which is what the relay checks the image for.
   A build that needs more fails here, not in an agent that will not start. *)
let glibc_floor_check =
  "newest=$(objdump -T /tmp/lf/src/libfaketimeMT.so.1 | grep -o \
   'GLIBC_[0-9][0-9.]*' | sed 's/GLIBC_//' | sort -V | tail -1) && [ \
   \"$(printf '%s\\n' \"$newest\" " ^ Vm_exec_probe.glibc_floor
  ^ " | sort -V | tail -1)\" = " ^ Vm_exec_probe.glibc_floor ^ " ]"

let faketime_builder : Vm_exec_probe.libc -> string * string = function
  | Musl ->
      ( build_image,
        faketime_build_script
          ~install:"apk add --no-cache make gcc musl-dev >&2" ~check:"true" )
  | Glibc ->
      ( glibc_build_image,
        faketime_build_script
          ~install:
            "apt-get update -qq >&2 && apt-get install -y -qq \
             --no-install-recommends gcc make libc6-dev binutils >&2"
          ~check:glibc_floor_check )

(* A directory beside the relay, named for everything the build depends on, so
   a changed version or script is built again. *)
let faketime_name t =
  Printf.sprintf "faketime-%s-%s"
    (String.sub
       Digest.BLAKE256.(
         to_hex
           (string
              (String.concat "\000"
                 ([ faketime_url; faketime_sha256; faketime_fetch_script ]
                 @ List.concat_map
                     (fun libc ->
                       let image, script = faketime_builder libc in
                       [ image; script ])
                     [ Vm_exec_probe.Musl; Glibc ]))))
       0 16)
    (String.map
       (fun c ->
         match c with
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> c
         | _ -> '-')
       t.arch)

let library_file = "libfaketimeMT.so.1"

let library_path ~parent libc =
  Filename.concat
    (Filename.concat parent (Vm_exec_probe.libc_name libc))
    library_file

(* A run command in the build image of a container, with [src] read-only and
   [out] writable. *)
let build_in ~process_mgr ~clock t ~what ~image ~src ~out script =
  let run =
    match t.kind with
    | `Docker -> [ "run"; "--rm" ]
    | `Container -> [ "run"; "--rm"; "--progress"; "none" ]
  in
  Result.map_error
    (fun why ->
      Printf.sprintf "%s could not be built in %s (%s). It needs the network"
        what image why)
    (run_client ~process_mgr ~clock ~timeout:build_timeout t
       (run
       @ [
           "--mount";
           Printf.sprintf "type=bind,source=%s,target=/src,readonly" src;
           "--mount";
           Printf.sprintf "type=bind,source=%s,target=/out" out;
           image;
           "sh";
           "-c";
           script;
         ]))

(* The archive, downloaded once into the cache. *)
let faketime_source ~process_mgr ~clock t ~dir ~work =
  let target = Filename.concat dir faketime_archive in
  match owned ~kind:S_REG ~what:"a regular file" target with
  | Error why -> Error why
  | Ok true -> Ok target
  | Ok false ->
      let src = Filename.concat work "fetch-src"
      and out = Filename.concat work "fetch-out" in
      List.iter (fun d -> Unix.mkdir d 0o755) [ src; out ];
      let* (_ : string) =
        build_in ~process_mgr ~clock t ~what:"libfaketime's source"
          ~image:build_image ~src ~out faketime_fetch_script
      in
      Unix.rename (Filename.concat out faketime_archive) target;
      Ok target

let faketime_trusted ~parent libc =
  let lib = library_path ~parent libc in
  let* parent_ok = owned ~kind:S_DIR ~what:"a directory" parent in
  if not parent_ok then Ok false
  else
    let* dir_ok =
      owned ~kind:S_DIR ~what:"a directory" (Filename.dirname lib)
    in
    if not dir_ok then Ok false
    else owned ~kind:S_REG ~what:"a regular file" lib

let faketime_cached t libc =
  let parent = Filename.concat (relay_cache_dir ()) (faketime_name t) in
  match faketime_trusted ~parent libc with
  | result -> result
  | exception Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)

let faketime ~process_mgr ~clock t libc =
  let dir = relay_cache_dir () in
  let parent = Filename.concat dir (faketime_name t) in
  let untrusted why =
    Error
      (Printf.sprintf
         "the cached libfaketime cannot be trusted: %s. Fix: remove %s" why
         parent)
  in
  match
    mkdirs dir;
    match owned ~kind:S_DIR ~what:"a directory" dir with
    | Error why -> untrusted why
    | Ok (_ : bool) -> (
        match faketime_trusted ~parent libc with
        | Error why -> untrusted why
        | Ok true -> Ok parent
        | Ok false -> (
            let work =
              Filename.concat dir
                ("build-" ^ Proc_util.hex (Mirage_crypto_rng.generate 6))
            in
            Fun.protect ~finally:(fun () ->
                try Proc_util.remove_tree work
                with Unix.Unix_error _ | Sys_error _ -> ())
            @@ fun () ->
            Unix.mkdir work 0o755;
            let* archive = faketime_source ~process_mgr ~clock t ~dir ~work in
            let src = Filename.concat work "src"
            and out = Filename.concat work "out" in
            List.iter (fun d -> Unix.mkdir d 0o755) [ src; out ];
            Out_channel.with_open_bin (Filename.concat src faketime_archive)
              (fun oc ->
                output_string oc
                  (In_channel.with_open_bin archive In_channel.input_all));
            let image, script = faketime_builder libc in
            let* (_ : string) =
              build_in ~process_mgr ~clock t ~what:"libfaketime" ~image ~src
                ~out script
            in
            mkdirs (Filename.dirname (library_path ~parent libc));
            Unix.rename
              (Filename.concat out library_file)
              (library_path ~parent libc);
            match faketime_trusted ~parent libc with
            | Ok true -> Ok parent
            | Ok false ->
                untrusted
                  (library_path ~parent libc ^ " is missing after the build")
            | Error why -> untrusted why))
  with
  | result -> result
  | exception ((Unix.Unix_error _ | Sys_error _) as e) ->
      Error
        (Printf.sprintf "libfaketime could not be built in %s: %s" dir
           (Printexc.to_string e))

let label_run = "bactrian.run"
let label_pid = "bactrian.pid"
let label_host = "bactrian.host"

let labels ~run_id =
  [
    (label_run, run_id);
    (label_pid, string_of_int (Unix.getpid ()));
    (label_host, Unix.gethostname ());
  ]

(* By its process ID only, never by a port: a process that cannot be signalled
   for want of permission is alive. *)
let alive pid =
  match Unix.kill pid 0 with
  | () -> true
  | exception Unix.Unix_error (ESRCH, _, _) -> false
  | exception Unix.Unix_error _ -> true

let owner_gone ~pid ~host =
  match int_of_string_opt pid with
  | Some pid ->
      pid > 0 && String.equal host (Unix.gethostname ()) && not (alive pid)
  | None -> false

(* A container of this host whose bactrian has gone. *)
let stale ~name ~pid ~host =
  String.starts_with ~prefix:"bactrian-" name && owner_gone ~pid ~host

(* Each container that carries the labels, as (name, pid, host). *)
let labelled ~process_mgr ~clock t =
  match t.kind with
  | `Docker ->
      Result.map
        (fun out ->
          List.filter_map
            (fun line ->
              match String.split_on_char '\t' line with
              | [ name; pid; host ] -> Some (name, pid, host)
              | _ -> None)
            (String.split_on_char '\n' out))
        (run_client ~process_mgr ~clock t
           [
             "ps";
             "--all";
             "--filter";
             "label=" ^ label_pid;
             "--format";
             Printf.sprintf {|{{.Names}}	{{.Label "%s"}}	{{.Label "%s"}}|}
               label_pid label_host;
           ])
  | `Container ->
      let* out =
        run_client ~process_mgr ~clock t [ "list"; "--all"; "--format"; "json" ]
      in
      let* listed = Container_cli.containers out in
      Ok
        (List.filter_map
           (fun (c : Container_cli.listed) ->
             match
               ( List.assoc_opt label_pid c.labels,
                 List.assoc_opt label_host c.labels )
             with
             | Some pid, Some host -> Some (c.id, pid, host)
             | Some _, None | None, (Some _ | None) -> None)
           listed)

let remove ~process_mgr ~clock t name =
  ignore
    (run_client ~process_mgr ~clock t
       (match t.kind with
       | `Docker -> [ "rm"; "-f"; name ]
       | `Container -> [ "delete"; "--force"; name ])
      : (string, string) result)

let remove_stale ~process_mgr ~clock t =
  match labelled ~process_mgr ~clock t with
  | Error _ -> []
  | Ok containers ->
      let names =
        List.filter_map
          (fun (name, pid, host) ->
            if stale ~name ~pid ~host then Some name else None)
          containers
      in
      List.iter (remove ~process_mgr ~clock t) names;
      names

let relay_cached t =
  let dir = relay_cache_dir () in
  match owned ~kind:S_DIR ~what:"a directory" dir with
  | Error why -> Error why
  | Ok false -> Ok false
  | Ok true ->
      owned ~kind:S_REG ~what:"a regular file"
        (Filename.concat dir (relay_name t))
  | exception Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)
