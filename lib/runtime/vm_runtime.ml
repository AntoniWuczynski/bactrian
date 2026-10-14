type t = {
  name : string;
  exe : string;
  client_env : string array;
  arch : string;
}

let name t = t.name
let exe t = t.exe
let client_env t = t.client_env
let arch t = t.arch
let ( let* ) = Result.bind

let client_variables =
  [
    "PATH";
    "HOME";
    "USER";
    "LOGNAME";
    "TMPDIR";
    "XDG_RUNTIME_DIR";
    "XDG_CONFIG_HOME";
    "DOCKER_HOST";
    "DOCKER_CONTEXT";
    "DOCKER_CONFIG";
    "DOCKER_CERT_PATH";
    "DOCKER_TLS_VERIFY";
    "DOCKER_API_VERSION";
  ]

let current_client_env () =
  Array.of_list
    (List.filter_map
       (fun k -> Option.map (fun v -> k ^ "=" ^ v) (Sys.getenv_opt k))
       client_variables)

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

let runtime_name : Bactrian_core.Experiment.sandbox_runtime -> string = function
  | `Docker -> "docker"

let detect ~process_mgr ~clock requested =
  let name = runtime_name (Option.value requested ~default:`Docker) in
  match
    Proc_util.find_executable ~path:(Sys.getenv_opt "PATH") ~cwd:(Sys.getcwd ())
      name
  with
  | None ->
      Error
        (Printf.sprintf
           "%s is not on PATH. Fix: install a Docker-compatible runtime \
            (OrbStack or Docker Desktop on macOS, Docker Engine on Linux; \
            Podman is not supported yet)"
           name)
  | Some exe -> (
      let client_env = current_client_env () in
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
          | arch -> Ok { name; exe; client_env; arch }))

let image_id ~process_mgr ~clock t image =
  match
    capture ~process_mgr ~clock ~env:t.client_env ~timeout:30.
      [ t.exe; "image"; "inspect"; "--format"; "{{.Id}}"; "--"; image ]
  with
  | Ok out when String.starts_with ~prefix:"sha256:" (String.trim out) ->
      Ok (String.trim out)
  | Ok out ->
      Error
        (Printf.sprintf "%s image inspect gave %S, not an image ID" t.name
           (String.trim out))
  | Error _ ->
      Error
        (Printf.sprintf "the image %s is not present locally. Fix: %s pull %s"
           image t.name image)

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
    (String.sub Digest.BLAKE256.(to_hex (string Relay_source.text)) 0 16)
    (String.map
       (fun c ->
         match c with
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> c
         | _ -> '-')
       t.arch)

(* Alpine 3.24.2 (2026-09-18), the multi-architecture index. *)
let build_image =
  "alpine:3.24.2@sha256:294b683cb724975bec92580e1e685676bd4b50bda910ddb8c51d4cabeaec77e6"

let build_timeout = 600.

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | S_DIR ->
      Array.iter
        (fun f -> remove_tree (Filename.concat path f))
        (Sys.readdir path);
      Unix.rmdir path
  | S_REG | S_LNK | S_CHR | S_BLK | S_FIFO | S_SOCK -> Unix.unlink path
  | exception Unix.Unix_error (ENOENT, _, _) -> ()

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
      try remove_tree work with Unix.Unix_error _ | Sys_error _ -> ())
  @@ fun () ->
  List.iter (fun d -> Unix.mkdir d 0o755) [ work; src; out ];
  Out_channel.with_open_bin (Filename.concat src "relay.c") (fun oc ->
      output_string oc Relay_source.text);
  let script =
    Printf.sprintf
      "apk add --no-cache gcc musl-dev >&2 && gcc -static -O2 -Wall -Wextra \
       -Werror -o /out/relay /src/relay.c && chown %d:%d /out/relay"
      (Unix.getuid ()) (Unix.getgid ())
  in
  let* _ =
    Result.map_error
      (fun why ->
        Printf.sprintf
          "the relay could not be built in %s (%s). It needs the network once, \
           for Alpine's packages"
          build_image why)
      (capture ~process_mgr ~clock ~env:t.client_env ~timeout:build_timeout
         [
           t.exe;
           "run";
           "--rm";
           "--mount";
           Printf.sprintf "type=bind,source=%s,target=/src,readonly" src;
           "--mount";
           Printf.sprintf "type=bind,source=%s,target=/out" out;
           build_image;
           "sh";
           "-c";
           script;
         ])
  in
  Unix.rename (Filename.concat out "relay") target;
  Ok target

let relay ~process_mgr ~clock t =
  let dir = relay_cache_dir () in
  let target = Filename.concat dir (relay_name t) in
  if Proc_util.regular_file target then Ok target
  else
    match
      mkdirs dir;
      build ~process_mgr ~clock t ~dir ~target
    with
    | result -> result
    | exception ((Unix.Unix_error _ | Sys_error _) as e) ->
        Error
          (Printf.sprintf "the relay could not be built in %s: %s" dir
             (Printexc.to_string e))

let remove ~process_mgr ~clock t name =
  ignore
    (capture ~process_mgr ~clock ~env:t.client_env ~timeout:30.
       [ t.exe; "rm"; "-f"; name ]
      : (string, string) result)
