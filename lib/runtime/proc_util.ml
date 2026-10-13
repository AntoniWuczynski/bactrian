let hex s =
  String.concat ""
    (List.init (String.length s) (fun i ->
         Printf.sprintf "%02x" (Char.code s.[i])))

let absolute ?cwd path =
  if Filename.is_relative path then
    Filename.concat (match cwd with Some d -> d | None -> Sys.getcwd ()) path
  else path

let regular_file path =
  match (Unix.stat path).st_kind with
  | S_REG -> true
  | S_DIR | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK -> false
  | exception Unix.Unix_error _ -> false

let executable_at path =
  if
    regular_file path
    &&
    match Unix.access path [ X_OK ] with
    | () -> true
    | exception Unix.Unix_error _ -> false
  then Some path
  else None

let find_executable ~path ~cwd name =
  if String.contains name '/' then executable_at (absolute ~cwd name)
  else
    String.split_on_char ':' (Option.value path ~default:"")
    |> List.find_map (fun dir ->
        if String.equal dir "" then None
        else executable_at (Filename.concat (absolute ~cwd dir) name))

(* OCaml numbers its portable signals negatively, so the number alone says
   nothing to a reader. *)
let signal_names =
  [
    (Sys.sigabrt, "SIGABRT");
    (Sys.sigalrm, "SIGALRM");
    (Sys.sigbus, "SIGBUS");
    (Sys.sigchld, "SIGCHLD");
    (Sys.sigcont, "SIGCONT");
    (Sys.sigfpe, "SIGFPE");
    (Sys.sighup, "SIGHUP");
    (Sys.sigill, "SIGILL");
    (Sys.sigint, "SIGINT");
    (Sys.sigkill, "SIGKILL");
    (Sys.sigpipe, "SIGPIPE");
    (Sys.sigpoll, "SIGPOLL");
    (Sys.sigprof, "SIGPROF");
    (Sys.sigquit, "SIGQUIT");
    (Sys.sigsegv, "SIGSEGV");
    (Sys.sigstop, "SIGSTOP");
    (Sys.sigsys, "SIGSYS");
    (Sys.sigterm, "SIGTERM");
    (Sys.sigtrap, "SIGTRAP");
    (Sys.sigtstp, "SIGTSTP");
    (Sys.sigttin, "SIGTTIN");
    (Sys.sigttou, "SIGTTOU");
    (Sys.sigurg, "SIGURG");
    (Sys.sigusr1, "SIGUSR1");
    (Sys.sigusr2, "SIGUSR2");
    (Sys.sigvtalrm, "SIGVTALRM");
    (Sys.sigxcpu, "SIGXCPU");
    (Sys.sigxfsz, "SIGXFSZ");
  ]

let signal_name n =
  match List.assoc_opt n signal_names with
  | Some name -> name
  | None -> Printf.sprintf "signal %d" n

let describe_exit : Eio.Process.exit_status -> string = function
  | `Exited n -> Printf.sprintf "exit status %d" n
  | `Signaled n -> "killed by " ^ signal_name n
