open Bactrian_core

type coverage = Seatbelt_log | Relay_failures_only

let coverage_name = function
  | Seatbelt_log -> "seatbelt-log"
  | Relay_failures_only -> "relay-failures-only"

let coverage_of_name = function
  | "seatbelt-log" -> Some Seatbelt_log
  | "relay-failures-only" -> Some Relay_failures_only
  | _ -> None

type violations =
  | Collected of {
      items : Sandbox_log.violation list;
      coverage : coverage;
      gaps : string list;
    }
  | Not_collected of string

let unsandboxed_violations = Not_collected "the run was not sandboxed"
let log_drops = "the system log can drop denials, so this is a lower bound"

let violations_json = function
  | Collected { items; coverage; gaps } ->
      `Assoc
        ([
           ("collected", `Bool true);
           ("coverage", `String (coverage_name coverage));
           ("complete", `Bool (List.is_empty gaps));
         ]
        @ (match gaps with
          | [] -> []
          | _ :: _ -> [ ("reason", `String (String.concat "; " gaps)) ])
        @ [
            ( "items",
              `List
                (List.map
                   (fun (v : Sandbox_log.violation) ->
                     `Assoc
                       [
                         ("kind", `String v.kind); ("detail", `String v.detail);
                       ])
                   items) );
          ])
  | Not_collected reason ->
      `Assoc
        [
          ("collected", `Bool false);
          ("reason", `String reason);
          ("items", `List []);
        ]

let plural n one = Printf.sprintf "%d %s%s" n one (if n = 1 then "" else "s")

let describe_violations = function
  | Not_collected reason -> "not collected: " ^ reason
  | Collected { items; coverage; gaps } ->
      let n = List.length items in
      let count, scope =
        match coverage with
        | Seatbelt_log ->
            ( (if n = 0 then "none logged" else "at least " ^ string_of_int n),
              [] )
        | Relay_failures_only ->
            ( plural n "relay failure",
              [ "Linux records no other kind of denial" ] )
      in
      Printf.sprintf "%s (%s)" count (String.concat "; " (scope @ gaps))

(* The forwarder writes one line per relay failure, so this much means
   something is flooding the log, and the rest is dropped (still read, so the
   writer never blocks). *)
let max_log = 16 * 1024 * 1024

(* Copies the forwarder's lines to [fd] until every write end of the pipe is
   closed. Whether any were dropped. *)
let drain reader fd =
  let r = Eio.Buf_read.of_flow ~max_size:max_log reader in
  let rec copy written dropped =
    match Eio.Buf_read.line r with
    | exception End_of_file -> dropped
    | exception Eio.Buf_read.Buffer_limit_exceeded -> true
    | line ->
        let text = line ^ "\n" in
        let n = String.length text in
        if written + n > max_log then copy written true
        else begin
          ignore (Unix.write_substring fd text 0 n : int);
          copy (written + n) dropped
        end
  in
  copy 0 false

let log_timeout = 30.

(* Denies the one write it makes, with the end tag, so the marker always
   reaches the log. *)
let marker_profile =
  "(version 1)\n\
   (deny default (with message (param \"TAG\")))\n\
   (allow process-exec)\n\
   (allow file-read*)\n"

let seatbelt ~env ~tag ~since ~marker =
  let mgr = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.clock env in
  let end_tag = Sandbox_log.end_tag tag in
  (* touch reports the denial on its standard error, which is not ours. *)
  let discard = Eio.Flow.buffer_sink (Buffer.create 256) in
  match
    Eio.Process.run mgr
      ~is_success:(fun (_ : int) -> true)
      ~stdout:discard ~stderr:discard ~env:(Proc_util.tool_env ())
      [
        "/usr/bin/sandbox-exec";
        "-p";
        marker_profile;
        "-D";
        "TAG=" ^ end_tag;
        "/usr/bin/touch";
        marker;
      ]
  with
  | exception Eio.Io _ -> Not_collected "the end marker could not be logged"
  | () -> (
      let argv =
        [
          "/usr/bin/log";
          "show";
          "--style";
          "ndjson";
          "--start";
          Printf.sprintf "@%d" (Float.to_int since - 1);
          "--predicate";
          Sandbox_log.predicate ~tag;
        ]
      in
      let read () =
        String.split_on_char '\n'
          (Eio.Process.parse_out mgr ~env:(Proc_util.tool_env ())
             Eio.Buf_read.take_all argv)
      in
      let now () = Eio.Time.now clock in
      (* The deadline is checked between reads, and the outer bound only stops
         a [log show] that hangs. *)
      let collected ?(gaps = []) items =
        Collected { items; coverage = Seatbelt_log; gaps = log_drops :: gaps }
      in
      match
        Eio.Time.with_timeout clock (log_timeout +. 5.) (fun () ->
            Ok
              (Sandbox_log.settle ~tag ~read ~wait:(Eio.Time.sleep clock) ~now
                 ~deadline:(now () +. log_timeout)))
      with
      | Ok (Settled vs) -> collected vs
      | Ok (Unsettled vs) ->
          collected vs
            ~gaps:
              [
                Printf.sprintf
                  "the system log was still changing after %.0f seconds"
                  log_timeout;
              ]
      | Ok No_marker ->
          Not_collected
            (Printf.sprintf
               "the system log did not show the run's end marker within %.0f \
                seconds"
               log_timeout)
      | Error `Timeout ->
          Not_collected
            (Printf.sprintf "log show did not finish within %.0f seconds"
               (log_timeout +. 5.))
      | exception (Eio.Io _ as e) ->
          Not_collected ("log show failed: " ^ Printexc.to_string e))

let relay_violations ~gaps path =
  match In_channel.with_open_bin path In_channel.input_all with
  | exception Sys_error m -> Not_collected m
  | text ->
      let items, bad = Sandbox_log.of_relay_log text in
      Collected
        {
          items;
          coverage = Relay_failures_only;
          gaps =
            (gaps
            @
            if bad = 0 then []
            else
              [
                Printf.sprintf "%s in sandbox.log %s skipped"
                  (plural bad "unreadable line")
                  (if bad = 1 then "was" else "were");
              ]);
        }

(* Every write end is gone once bubblewrap has exited, so this is a bound on a
   forwarder that did not, never a wait in a normal run. *)
let drain_timeout = 5.

let relay ~env ~path ~writer ~drained =
  Eio.Resource.close writer;
  match
    Eio.Time.with_timeout (Eio.Stdenv.clock env) drain_timeout (fun () ->
        Ok (Eio.Promise.await_exn drained))
  with
  | Ok dropped ->
      relay_violations path
        ~gaps:
          (if dropped then
             [
               Printf.sprintf "the log passed %d bytes, the rest was dropped"
                 max_log;
             ]
           else [])
  | Error `Timeout ->
      relay_violations path
        ~gaps:[ "a process still held the log open at the end of the run" ]
  | exception ((Eio.Io _ | Unix.Unix_error _) as e) ->
      Not_collected ("sandbox.log: " ^ Printexc.to_string e)
