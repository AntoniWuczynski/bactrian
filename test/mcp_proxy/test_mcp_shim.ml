open Bactrian_core
open Mcp_support

let bactrian = Filename.concat (Sys.getcwd ()) (Sys.getenv "BACTRIAN_BIN")

(* Runs [bactrian mcp-shim weather] with [env] as its whole environment, writes
   [lines] to its stdin, closes it, and returns the exit status and every line
   of stdout. *)
let shim ctx ~env lines =
  Eio.Switch.run @@ fun sw ->
  let in_r, in_w = Eio_unix.pipe sw and out_r, out_w = Eio_unix.pipe sw in
  let proc =
    Eio.Process.spawn ~sw
      (Eio.Stdenv.process_mgr ctx.env)
      ~stdin:in_r ~stdout:out_w ~env:(Array.of_list env)
      [ bactrian; "mcp-shim"; "weather" ]
  in
  Eio.Resource.close in_r;
  Eio.Resource.close out_w;
  (* A shim that refuses to start may exit before reading its input. *)
  (try List.iter (fun l -> Eio.Flow.copy_string (l ^ "\n") in_w) lines
   with Eio.Io (Eio.Net.E (Connection_reset _), _) -> ());
  Eio.Resource.close in_w;
  let out =
    Eio.Buf_read.parse_exn Eio.Buf_read.take_all ~max_size:max_int out_r
  in
  let status =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock ctx.env) 10. (fun () ->
        Eio.Process.await proc)
  in
  (status, List.filter (fun l -> l <> "") (String.split_on_char '\n' out))

let url_env ctx ~token = [ M.url_variable ^ "=" ^ P.mcp_url ctx.proxy ~token ]

let ids lines =
  List.map
    (fun l ->
      match decode l with
      | Response { id; _ } | Error_response { id = Some id; _ } -> id
      | Error_response { id = None; _ } | Request _ | Notification _ ->
          Alcotest.failf "unexpected line %s" l)
    lines
  |> List.sort (fun (a : Mcp_msg.id) b ->
      match (a, b) with
      | Int_id x, Int_id y -> Int.compare x y
      | Int_id _, String_id _ -> -1
      | String_id _, Int_id _ -> 1
      | String_id x, String_id y -> String.compare x y)

let exited_zero = function
  | `Exited 0 -> ()
  | `Exited n -> Alcotest.failf "shim exited %d" n
  | `Signaled n -> Alcotest.failf "shim killed by %d" n

let session =
  [
    discover (Int_id 1);
    initialized;
    "   ";
    call (Int_id 2) "Paris";
    call (Int_id 3) "London";
  ]

let round_trip =
  Alcotest.test_case "record and replay through the shim binary" `Slow
  @@ with_ctx "mcp-shim"
  @@ fun ctx ->
  let server = stdio "weather" in
  let run = record_run ctx ~token:token_a [ server ] in
  let status, out = shim ctx ~env:(url_env ctx ~token:token_a) session in
  exited_zero status;
  Alcotest.(list id_testable |> check)
    "one reply per request"
    [ Int_id 1; Int_id 2; Int_id 3 ]
    (ids out);
  ignore (P.unregister ctx.proxy ~token:token_a);
  let _ = replay_mcp ctx ~token:token_b (recording ctx run) [ server ] in
  let status, replayed = shim ctx ~env:(url_env ctx ~token:token_b) session in
  exited_zero status;
  Alcotest.(check (list string))
    "same replies"
    (List.sort String.compare out)
    (List.sort String.compare replayed);
  no_divergence (P.unregister ctx.proxy ~token:token_b)

let crash =
  Alcotest.test_case "a crashed server does not hang the shim" `Slow
  @@ with_ctx "mcp-shim-crash"
  @@ fun ctx ->
  let _ =
    record_run ctx ~token:token_a
      [ stdio ~args:[ "--crash-on"; "Atlantis" ] "weather" ]
  in
  let status, out =
    shim ctx
      ~env:(url_env ctx ~token:token_a)
      [ call (Int_id 1) "Atlantis"; call (Int_id 2) "Paris" ]
  in
  exited_zero status;
  (* The two calls run in parallel, so only the crashing one must fail. *)
  let atlantis =
    List.filter
      (fun l ->
        match decode l with
        | Response { id; _ } | Error_response { id = Some id; _ } ->
            Alcotest.equal id_testable id (Int_id 1)
        | Error_response { id = None; _ } | Request _ | Notification _ -> false)
      out
  in
  let _, code, _, _ = error_of (only atlantis) in
  Alcotest.(check int) "code" (-32000) code;
  Alcotest.(list id_testable |> check)
    "both answered" [ Int_id 1; Int_id 2 ] (ids out);
  ignore (P.unregister ctx.proxy ~token:token_a)

let refused =
  Alcotest.test_case "an ended run and a missing URL" `Slow
  @@ with_ctx "mcp-shim-refused"
  @@ fun ctx ->
  let status, out =
    shim ctx
      ~env:(url_env ctx ~token:token_c)
      [ call (Int_id 4) "Paris"; initialized ]
  in
  exited_zero status;
  (match out with
  | [ line ] ->
      let id, _, message, _ = error_of line in
      Alcotest.check id_testable "id" (Int_id 4) id;
      if not (contains message "403") then Alcotest.failf "message %s" message
  | _ -> Alcotest.failf "expected one line, got %d" (List.length out));
  let status, out = shim ctx ~env:[] [ call (Int_id 5) "Paris" ] in
  (match status with
  | `Exited 0 -> Alcotest.fail "the shim succeeded without its URL"
  | `Exited _ | `Signaled _ -> ());
  Alcotest.(check (list string)) "nothing on stdout" [] out

(* A 2025-11-25 client sends [initialize], waits for its answer, then sends
   [notifications/initialized] and its first request back to back. The legacy
   fake refuses any request before [notifications/initialized], as SDK servers
   do, so the order the server sees is the order the agent wrote. *)
let legacy_handshake ctx ~token =
  Eio.Switch.run @@ fun sw ->
  let in_r, in_w = Eio_unix.pipe sw and out_r, out_w = Eio_unix.pipe sw in
  let proc =
    Eio.Process.spawn ~sw
      (Eio.Stdenv.process_mgr ctx.env)
      ~stdin:in_r ~stdout:out_w
      ~env:(Array.of_list (url_env ctx ~token))
      [ bactrian; "mcp-shim"; "weather" ]
  in
  Eio.Resource.close in_r;
  Eio.Resource.close out_w;
  let out = Eio.Buf_read.of_flow ~max_size:max_int out_r in
  Eio.Flow.copy_string (initialize (Int_id 1) ^ "\n") in_w;
  ignore (result_of (Eio.Buf_read.line out));
  Eio.Flow.copy_string
    (initialized ^ "\n" ^ tools_list ~era:Legacy (Int_id 2) ^ "\n")
    in_w;
  let listed = Eio.Buf_read.line out in
  Eio.Resource.close in_w;
  exited_zero
    (Eio.Time.with_timeout_exn (Eio.Stdenv.clock ctx.env) 10. (fun () ->
         Eio.Process.await proc));
  listed

let methods lines =
  List.map
    (fun l ->
      match decode l with
      | Request { method_; _ } | Notification { method_; _ } -> method_
      | Response _ | Error_response _ -> Alcotest.failf "unexpected line %s" l)
    lines

let handshake_order =
  Alcotest.test_case "a legacy handshake reaches the server in order" `Slow
  @@ with_ctx "mcp-shim-order"
  @@ fun ctx ->
  (* The notification and the request used to race, so one attempt proves
     little: each round is a fresh run and a fresh server. *)
  for round = 1 to 20 do
    let token = Printf.sprintf "%04d" round ^ String.make 28 'e' in
    let log = path ctx (Printf.sprintf "server-%d.log" round) in
    P.register ctx.proxy
      (make_run ~record:false
         ~mcp:[ stdio ~era:Legacy ~args:[ "--log"; log ] "weather" ]
         ctx ~token ~upstream:no_upstream);
    let listed = legacy_handshake ctx ~token in
    ignore (P.unregister ctx.proxy ~token);
    Alcotest.(check (list string))
      (Printf.sprintf "round %d: the server's order" round)
      [ "initialize"; "notifications/initialized"; "tools/list" ]
      (methods (lines_of log));
    let id, _ = result_of listed in
    Alcotest.check id_testable "listed" (Int_id 2) id
  done

let unknown_server =
  Alcotest.test_case "a server the run does not have answers with the id" `Slow
  @@ with_ctx "mcp-shim-unknown"
  @@ fun ctx ->
  let _ = record_run ctx ~token:token_a [ stdio "other" ] in
  let status, out =
    shim ctx ~env:(url_env ctx ~token:token_a) [ call (Int_id 6) "Paris" ]
  in
  exited_zero status;
  let id, code, _, _ = error_of (only out) in
  Alcotest.check id_testable "id" (Int_id 6) id;
  Alcotest.(check int) "code" (-32601) code;
  ignore (P.unregister ctx.proxy ~token:token_a)

(* An error the agent could not match to its request would leave it waiting:
   the shim gives it the request's id. *)
let null_id =
  Alcotest.test_case "an error reply without an id gets the request's id" `Quick
  @@ with_ctx "mcp-shim-null-id"
  @@ fun ctx ->
  let body =
    {|{"jsonrpc":"2.0","id":null,"error":{"code":-32700,"message":"bad"}}|}
  in
  let url =
    raw_upstream ctx
      ~reply:
        (Printf.sprintf
           "HTTP/1.1 400 Bad Request\r\n\
            content-type: application/json\r\n\
            content-length: %d\r\n\
            \r\n\
            %s"
           (String.length body) body)
  in
  let out = Buffer.create 256 in
  Bactrian_runtime.Mcp_shim.run ~net:(Eio.Stdenv.net ctx.env)
    ~clock:(Eio.Stdenv.clock ctx.env)
    ~stdin:(Eio.Flow.string_source (call (Int_id 8) "Paris" ^ "\n"))
    ~stdout:(Eio.Flow.buffer_sink out) ~url ~name:"weather";
  let id, code, message, _ = error_of (String.trim (Buffer.contents out)) in
  Alcotest.check id_testable "id" (Int_id 8) id;
  Alcotest.(check int) "code" (-32000) code;
  if not (contains message "bad") then Alcotest.failf "message %s" message

let () =
  Alcotest.run "mcp_shim"
    [
      ( "shim",
        [ round_trip; crash; refused; handshake_order; unknown_server; null_id ]
      );
    ]
