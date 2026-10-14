open Bactrian_core
module P = Model_proxy

type status = Pass | Warn | Fail | Skip

let status_name = function
  | Pass -> "ok"
  | Warn -> "warn"
  | Fail -> "fail"
  | Skip -> "skip"

let status_to_yojson s = `String (status_name s)

type check = { name : string; status : status; message : string }
[@@deriving to_yojson]

type report = { checks : check list; ok : bool } [@@deriving to_yojson]

let report checks =
  {
    checks;
    ok =
      List.for_all
        (fun c ->
          match c.status with Fail -> false | Pass | Warn | Skip -> true)
        checks;
  }

let to_json r = report_to_yojson r

let render_check c =
  match String.split_on_char '\n' c.message with
  | [] -> ""
  | first :: rest ->
      String.concat "\n"
        (Printf.sprintf "%-5s %s: %s" (status_name c.status) c.name first
        :: List.map (fun l -> "      " ^ l) rest)

let render_text r =
  String.concat "" (List.map (fun c -> render_check c ^ "\n") r.checks)

type target = {
  agent : Experiment.agent;
  dir : string;
  sandbox : Sandbox.mode;
  backend : Sandbox.backend;
  image : string option;
  runtime : Experiment.sandbox_runtime option;
  faketime : bool;
  funded : (P.provider * string) list;
}

let target ~dir (e : Experiment.t) =
  {
    agent = e.agent;
    dir;
    sandbox = e.sandbox;
    backend = e.sandbox_backend;
    image = e.sandbox_image;
    runtime = e.sandbox_runtime;
    faketime = e.faketime;
    funded = Live_preflight.funded e.caps_usd;
  }

let check name status message = { name; status; message }

(* Provider keys *)

let key_checks ~keys ~funded =
  List.map
    (fun (p, provider) ->
      let variable = Run.key_variable p in
      let name = "key-" ^ provider in
      if List.mem p keys then check name Pass (variable ^ " is set")
      else
        match funded with
        | None -> check name Warn (variable ^ " is not set")
        | Some funded ->
            if List.mem_assoc p funded then
              check name Fail
                (Printf.sprintf
                   "%s is not set, and the experiment has a spend cap for %s"
                   variable provider)
            else
              check name Skip
                (Printf.sprintf
                   "%s is not set, and the experiment has no spend cap for %s"
                   variable provider))
    Run.providers

(* Spend caps *)

let caps_check (t : target) =
  match t.funded with
  | [] ->
      check "caps" Fail
        (Live_preflight.describe ~set_caps:"caps_usd in the experiment"
           Live_preflight.No_cap)
  | _ :: _ ->
      check "caps" Pass
        ("a spend cap above 0 for " ^ String.concat ", " (List.map snd t.funded))

(* Upstream overrides *)

let upstream_checks ~upstream =
  List.map
    (fun (name, url) ->
      let shown = Uri.to_string url in
      let check_name = "upstream-" ^ name in
      if Upstream.is_https url || Live_preflight.is_loopback url then
        check check_name Pass shown
      else
        check check_name Warn
          (shown
         ^ " is plain HTTP to another machine: the real API key is sent \
            unencrypted"))
    (Run.overridden_upstreams upstream)

(* The agent *)

let http_url_problem url =
  let uri = Uri.of_string url in
  match (Uri.scheme uri, Uri.host uri) with
  | Some ("http" | "https"), Some host when not (String.equal host "") -> None
  | Some ("http" | "https"), (Some _ | None) -> Some "has no host"
  | Some scheme, _ -> Some (Printf.sprintf "has the scheme %S, not http" scheme)
  | None, _ -> Some "has no scheme (http:// or https://)"

(* A URL can carry a password or a token, and this output ends up in terminal
   logs and CI: only the scheme, host, port and path are shown. *)
let shown url =
  Uri.to_string
    (Uri.with_fragment
       (Uri.with_query (Uri.with_userinfo (Uri.of_string url) None) [])
       None)

let agent_check ~path ~cwd : Experiment.agent -> check = function
  | Command { argv; _ } -> (
      match argv with
      | [] -> check "agent" Fail "the agent command is empty"
      | argv0 :: _ -> (
          match Proc_util.find_executable ~path ~cwd argv0 with
          | Some file -> check "agent" Pass (argv0 ^ " resolves to " ^ file)
          | None ->
              check "agent" Fail
                (Printf.sprintf
                   "%s is not an executable file on PATH or relative to %s"
                   argv0 cwd)))
  | Http { url; _ } -> (
      match http_url_problem url with
      | None ->
          check "agent" Pass
            (shown url ^ " is a well-formed URL (not contacted)")
      | Some problem ->
          check "agent" Fail (Printf.sprintf "%s %s" (shown url) problem))

(* The loopback *)

let loopback_check () =
  match
    let socket = Unix.socket PF_INET SOCK_STREAM 0 in
    Fun.protect
      ~finally:(fun () -> Unix.close socket)
      (fun () -> Unix.bind socket (ADDR_INET (Unix.inet_addr_loopback, 0)))
  with
  | () -> check "loopback" Pass "a port on 127.0.0.1 can be bound"
  | exception Unix.Unix_error (e, _, _) ->
      check "loopback" Fail
        ("cannot bind a port on 127.0.0.1: " ^ Unix.error_message e)

(* The sandbox *)

let sandbox_check ~(mode : Sandbox.mode) ~http_agent ~probe =
  if http_agent then
    check "sandbox" Skip "an HTTP-mode agent is not started by bactrian"
  else
    match mode with
    | `Off -> check "sandbox" Skip "sandbox: off"
    | (`Auto | `Strict) as mode -> (
        match probe () with
        | Ok () -> check "sandbox" Pass "the sandbox works on this host"
        | Error cause ->
            let status = match mode with `Strict -> Fail | `Auto -> Warn in
            check "sandbox" status
              (Printf.sprintf "sandbox: %s: %s" (Sandbox.mode_name mode) cause))

(* Runs the preflight probe in a directory of its own, so the check leaves no
   [.bactrian] behind. *)
let sandbox_probe ~env ~bactrian_exe (target : target option) () =
  let mode, backend, image, runtime =
    match target with
    | Some t -> (t.sandbox, t.backend, t.image, t.runtime)
    | None -> (`Auto, `Auto, None, None)
  in
  match Filename.temp_dir ~perms:0o700 "bactrian-doctor-" "" with
  | exception Sys_error m -> Error m
  | dir -> (
      Fun.protect ~finally:(fun () ->
          Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / dir))
      @@ fun () ->
      match
        Sandbox.preflight ~env
          ~warn:(fun (_ : string) -> ())
          {
            mode =
              (match mode with `Strict -> `Strict | `Auto | `Off -> `Auto);
            backend;
            image;
            runtime;
            allow_write = [];
            deny_read = [];
            bactrian_exe;
            bactrian_dir = Filename.concat dir ".bactrian";
          }
      with
      | Error cause -> Error cause
      | Ok sandbox -> (
          match Sandbox.label sandbox with
          | { applied = true; _ } -> Ok ()
          | { reason; _ } -> Error (Option.value reason ~default:"unavailable"))
      )

(* The virtual machine *)

type vm_host = {
  macos : bool;
  detect : unit -> (string, string) result;
      (** The runtime's name and architecture, or why it does not work. *)
  image_present : string -> (unit, string) result;
  relay_cached : unit -> bool;
}

let no_runtime_fix =
  "install OrbStack or Docker Desktop (Podman is not supported yet)"

(* Whether the experiment's agent would run in the virtual machine. *)
let uses_vm ~macos (t : target) =
  match (t.agent, t.sandbox, t.backend) with
  | Http _, _, _ | Command _, `Off, _ -> false
  | Command _, (`Auto | `Strict), `Vm -> true
  | Command _, (`Auto | `Strict), `Auto -> macos && Option.is_some t.image
  | Command _, (`Auto | `Strict), (`Seatbelt | `Bubblewrap) -> false

let vm_check ~host target =
  let needed, mode, image =
    match target with
    | None -> (false, `Auto, None)
    | Some { agent = Http _; _ } | Some { sandbox = `Off; _ } ->
        (false, `Off, None)
    | Some ({ agent = Command _; _ } as t) ->
        (uses_vm ~macos:host.macos t, t.sandbox, t.image)
  in
  let failed what =
    let status =
      match mode with `Strict -> Fail | `Auto -> Warn | `Off -> Skip
    in
    check "vm" status what
  in
  match (mode, target) with
  | `Off, _ -> check "vm" Skip "the agent is not sandboxed"
  | (`Auto | `Strict), Some { backend = `Seatbelt | `Bubblewrap; _ } ->
      check "vm" Skip "the experiment names the host's own sandbox"
  | (`Auto | `Strict), (Some _ | None) -> (
      match host.detect () with
      | Error why when needed -> failed why
      | Error why when host.macos ->
          check "vm" Warn
            (Printf.sprintf
               "no virtual machine sandbox, so an agent under Seatbelt can \
                read the launch environment of this user's other processes: \
                %s. Fix: %s"
               why no_runtime_fix)
      | Error _ -> check "vm" Skip "no container runtime (bubblewrap is used)"
      | Ok runtime -> (
          let relay =
            if host.relay_cached () then "the relay is built"
            else "the relay is built on first use (it needs the network once)"
          in
          match image with
          | None when host.macos && needed -> failed "no sandbox.image"
          | None when host.macos && Option.is_some target ->
              check "vm" Warn
                (Printf.sprintf
                   "%s works, but sandbox.image is not set, so the agent runs \
                    under Seatbelt and can read the launch environment of this \
                    user's other processes"
                   runtime)
          | None ->
              check "vm" Pass (Printf.sprintf "%s works, %s" runtime relay)
          | Some image -> (
              match host.image_present image with
              | Error why -> failed why
              | Ok () ->
                  check "vm" Pass
                    (Printf.sprintf "%s works, %s is present, %s" runtime image
                       relay))))

let vm_host ~env : vm_host =
  let process_mgr = Eio.Stdenv.process_mgr env
  and clock = Eio.Stdenv.clock env in
  let runtime = lazy (Vm_runtime.detect ~process_mgr ~clock None) in
  {
    macos = Sys.file_exists "/usr/bin/sandbox-exec";
    detect =
      (fun () ->
        Result.map
          (fun r ->
            Printf.sprintf "%s (%s)" (Vm_runtime.name r) (Vm_runtime.arch r))
          (Lazy.force runtime));
    image_present =
      (fun image ->
        Result.bind (Lazy.force runtime) (fun r ->
            Vm_runtime.image_present ~process_mgr ~clock r image));
    relay_cached =
      (fun () ->
        match Lazy.force runtime with
        | Ok r ->
            Proc_util.regular_file
              (Filename.concat
                 (Vm_runtime.relay_cache_dir ())
                 (Vm_runtime.relay_name r))
        | Error _ -> false);
  }

(* The clock *)

(* A time every [decide] accepts: the label does not depend on the value. *)
let any_sim_now = "2000-01-01T00:00:00Z"

let reason_status : Clock_label.reason -> status = function
  | Macos | Vm -> Pass
  | Faketime_off | Http_mode -> Skip
  | Library_not_found | Library_hidden | Setuid_sandbox | Invalid_sim_now
  | Static_binary | Go_binary | Unreadable_executable ->
      Warn

let reason_text : Clock_label.reason -> string = function
  | Macos -> "the system clock is not faked on macOS"
  | Faketime_off -> "the experiment turns faketime off"
  | Library_not_found ->
      "libfaketime is not found (install it, or set "
      ^ Agent_clock.override_variable ^ ")"
  | Library_hidden -> "the sandbox hides libfaketime from the agent"
  | Setuid_sandbox -> "a setuid bwrap drops LD_PRELOAD before the agent starts"
  | Invalid_sim_now -> "SIM_NOW is not an RFC 3339 time"
  | Static_binary -> "a statically linked agent ignores LD_PRELOAD"
  | Go_binary -> "Go programs read the clock without the C library"
  | Unreadable_executable ->
      "the agent's executable is missing, unreadable or not a little-endian \
       ELF file"
  | Http_mode -> "bactrian does not start an HTTP-mode agent"
  | Vm -> "the agent runs in the virtual machine sandbox"

let clock_check ~linux ~library_override ~path target =
  let contract_only reason =
    check "clock" (reason_status reason)
      (Printf.sprintf "contract-only (%s): %s"
         (Clock_label.reason_name reason)
         (reason_text reason))
  in
  match target with
  | Some { agent = Http _; _ } -> contract_only Http_mode
  | Some ({ agent = Command { argv; _ }; dir; faketime; _ } as t) -> (
      let label =
        Agent_clock.label
          (Agent_clock.decide
             ~vm:(uses_vm ~macos:(not linux) t)
             ~linux ~faketime ~library_override
             ~env:(match path with Some p -> [ ("PATH", p) ] | None -> [])
             ~cwd:dir ~agent:argv ~sim_now:any_sim_now ())
      in
      match label with
      | Enforced ->
          check "clock" Pass
            "enforced (libfaketime is preloaded, unless the experiment's \
             sandbox.deny_read hides it)"
      | Contract_only reason -> contract_only reason)
  | None -> (
      if not linux then contract_only Macos
      else
        match Agent_clock.locate ~override:library_override () with
        | Some library -> check "clock" Pass ("libfaketime found at " ^ library)
        | None -> contract_only Library_not_found)

(* TLS *)

let tls_check ~available ~upstream target =
  if available then check "tls" Pass "HTTPS support is built in"
  else
    let needed =
      match target with
      | None -> []
      | Some t -> (
          List.filter_map
            (fun (p, name) ->
              if Upstream.is_https (upstream p) then Some name else None)
            t.funded
          @
          match t.agent with
          | Http { url; _ } when Upstream.is_https (Uri.of_string url) ->
              [ "the agent at " ^ url ]
          | Http _ | Command _ -> [])
    in
    match needed with
    | [] -> check "tls" Skip "HTTPS support is not built in"
    | _ :: _ ->
        check "tls" Fail
          (Printf.sprintf
             "HTTPS support is not built in, but the experiment needs it for %s"
             (String.concat ", " needed))

(* Egress *)

let egress_checks =
  [
    check "ca" Skip "egress interception is not built yet";
    check "node" Skip "egress interception is not built yet";
  ]

(* All of them *)

let run ~env ~bactrian_exe ~tls_available ~upstream ~keys ~path target =
  let keys = List.map fst keys in
  let cwd = match target with Some t -> t.dir | None -> Sys.getcwd () in
  let http_agent =
    match target with
    | Some { agent = Http _; _ } -> true
    | Some { agent = Command _; _ } | None -> false
  in
  let mode = match target with Some t -> t.sandbox | None -> `Auto in
  key_checks ~keys ~funded:(Option.map (fun (t : target) -> t.funded) target)
  @ (match target with Some t -> [ caps_check t ] | None -> [])
  @ upstream_checks ~upstream
  @ [ loopback_check () ]
  @ (match target with
    | Some t when uses_vm ~macos:(Sys.file_exists "/usr/bin/sandbox-exec") t ->
        [
          check "agent" Skip
            (Printf.sprintf
               "runs in the image %s, so it is not looked up on this host"
               (Option.value t.image ~default:"(none)"));
        ]
    | Some t -> [ agent_check ~path ~cwd t.agent ]
    | None -> [])
  @ [
      sandbox_check ~mode ~http_agent
        ~probe:(sandbox_probe ~env ~bactrian_exe target);
      vm_check ~host:(vm_host ~env) target;
      clock_check ~linux:(Agent_clock.linux ())
        ~library_override:(Sys.getenv_opt Agent_clock.override_variable)
        ~path target;
      tls_check ~available:tls_available ~upstream target;
    ]
  @ egress_checks
