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

let regular_file path =
  match (Unix.stat path).st_kind with
  | S_REG -> true
  | S_DIR | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK -> false
  | exception Unix.Unix_error _ -> false

let locate ?(candidates = candidates) ~override () =
  match override with
  | Some path when not (String.equal path "") ->
      if regular_file path then Some path else None
  | Some _ | None -> List.find_opt regular_file candidates

let linux () = Sys.file_exists "/proc/sys/kernel/ostype"

(* Executables *)

let executable_file path =
  regular_file path
  &&
  match Unix.access path [ X_OK ] with
  | () -> true
  | exception Unix.Unix_error _ -> false

let in_cwd ~cwd path =
  if Filename.is_relative path then Filename.concat cwd path else path

let executable_at path = if executable_file path then Some path else None

let find ~path ~cwd name =
  if String.contains name '/' then executable_at (in_cwd ~cwd name)
  else
    String.split_on_char ':' (Option.value path ~default:"")
    |> List.find_map (fun dir ->
        if String.equal dir "" then None
        else executable_at (Filename.concat (in_cwd ~cwd dir) name))

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
  match find ~path ~cwd argv0 with
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
              match find ~path ~cwd program with
              | Some interpreter -> kind_of interpreter
              | None -> Elf_probe.Not_elf)))

(* The decision *)

type injection = { library : string; epoch : int }
type t = { label : Clock_label.t; injection : injection option }

let label t = t.label
let http_mode = { label = Contract_only Http_mode; injection = None }

let decide ?candidates ~linux ~faketime ~library_override ~env ~cwd ~agent
    ~sim_now () =
  let ( let* ) = Result.bind in
  let checked =
    let* () = if linux then Ok () else Error Clock_label.Macos in
    let* () = if faketime then Ok () else Error Clock_label.Faketime_off in
    let* library =
      Option.to_result ~none:Clock_label.Library_not_found
        (locate ?candidates ~override:library_override ())
    in
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
