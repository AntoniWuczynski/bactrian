open Bactrian_core

let override_variable = "BACTRIAN_LIBFAKETIME"

let candidates =
  List.map
    (fun dir -> dir ^ "/libfaketimeMT.so.1")
    [
      "/usr/lib/x86_64-linux-gnu/faketime";
      "/usr/lib/aarch64-linux-gnu/faketime";
      "/usr/lib/faketime";
      "/usr/local/lib/faketime";
    ]

(* The sandbox binds the library at the path the preload names, so that path
   is the resolved one. *)
let resolved path =
  match Unix.realpath path with
  | real -> Some real
  | exception Unix.Unix_error _ -> None

let locate ?(candidates = candidates) ~override () =
  let found =
    match override with
    | Some path when not (String.equal path "") ->
        if Proc_util.regular_file path then Some path else None
    | Some _ | None -> List.find_opt Proc_util.regular_file candidates
  in
  Option.bind found resolved

let linux () = Sys.file_exists "/proc/sys/kernel/ostype"

(* Executables *)

let read_prefix file =
  match
    In_channel.with_open_bin file (fun ic ->
        In_channel.really_input_string ic
          (min Elf_probe.prefix_length (Int64.to_int (In_channel.length ic))))
  with
  | contents -> contents
  | exception Sys_error _ -> None

let probe_executable ~path ~cwd argv0 =
  let kind_of file =
    match read_prefix file with
    | Some prefix -> Elf_probe.probe prefix
    | None -> Elf_probe.Not_elf
  in
  match Proc_util.find_executable ~path ~cwd argv0 with
  | None -> Elf_probe.Not_elf
  | Some file -> (
      match read_prefix file with
      | None -> Elf_probe.Not_elf
      | Some prefix -> (
          match Shebang.parse prefix with
          | None -> Elf_probe.probe prefix
          | Some script -> (
              let program =
                match script with Interpreter p | Env_program p -> p
              in
              match Proc_util.find_executable ~path ~cwd program with
              | Some interpreter -> kind_of interpreter
              | None -> Elf_probe.Not_elf)))

(* The decision *)

type injection = { library : string; epoch : int }
type t = { label : Clock_label.t; injection : injection option }

let label t = t.label
let http_mode = { label = Contract_only Http_mode; injection = None }
let library t = Option.map (fun i -> i.library) t.injection

let decide ?candidates ?(vm = false) ?(hidden = fun (_ : string) -> false)
    ?(stripped = false) ~linux ~faketime ~library_override ~env ~cwd ~agent
    ~sim_now () =
  let ( let* ) = Result.bind in
  let checked =
    let* () = if vm then Error Clock_label.Vm else Ok () in
    let* () = if linux then Ok () else Error Clock_label.Macos in
    let* () = if faketime then Ok () else Error Clock_label.Faketime_off in
    let* library =
      Option.to_result ~none:Clock_label.Library_not_found
        (locate ?candidates ~override:library_override ())
    in
    let* () =
      if hidden library then Error Clock_label.Library_hidden else Ok ()
    in
    let* () = if stripped then Error Clock_label.Setuid_sandbox else Ok () in
    let* epoch =
      match Rfc3339.epoch sim_now with
      | Some e when e >= 0 -> Ok e
      | Some _ | None -> Error Clock_label.Invalid_sim_now
    in
    let kind =
      match agent with
      | [] -> Elf_probe.Not_elf
      | argv0 :: _ ->
          probe_executable ~path:(List.assoc_opt "PATH" env) ~cwd argv0
    in
    match Clock_label.of_kind kind with
    | Enforced -> Ok { library; epoch }
    | Contract_only reason -> Error reason
  in
  match checked with
  | Ok injection -> { label = Enforced; injection = Some injection }
  | Error reason -> { label = Contract_only reason; injection = None }

(* Injection *)

let preload ~library existing =
  match existing with
  | None | Some "" -> library
  | Some v ->
      let entries =
        String.split_on_char ' ' v |> List.concat_map (String.split_on_char ':')
      in
      if List.mem library entries then v else library ^ ":" ^ v

let inject t env =
  match t.injection with
  | None -> env
  | Some { library; epoch } ->
      let set =
        [
          ("LD_PRELOAD", preload ~library (List.assoc_opt "LD_PRELOAD" env));
          ("FAKETIME", "@" ^ string_of_int epoch);
          ("FAKETIME_FMT", "%s");
          ("FAKETIME_DONT_RESET", "1");
        ]
      in
      List.filter (fun (k, _) -> not (List.mem_assoc k set)) env @ set
