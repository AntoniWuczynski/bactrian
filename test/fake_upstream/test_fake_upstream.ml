module F = Fake_upstream
module Canon = Bactrian_core.Canon

let fixtures_root = "../fixtures/protocols"

type response = {
  status : int;
  content_type : string option;
  chunks : string list;
  failure : exn option;  (** Set when reading the body ended in an exception. *)
}

let body_of r = String.concat "" r.chunks

(* Read the body one [single_read] at a time so chunk boundaries stay visible. *)
let post ~sw env ?(headers = []) ?(query = "") base path body =
  let client = Cohttp_eio.Client.make ~https:None (Eio.Stdenv.net env) in
  let url = base ^ path ^ if query = "" then "" else "?" ^ query in
  let headers = Http.Header.of_list headers in
  let resp, source =
    Cohttp_eio.Client.post ~sw ~headers
      ~body:(Cohttp_eio.Body.of_string body)
      client (Uri.of_string url)
  in
  let buf = Cstruct.create 65536 in
  let rec loop acc =
    match Eio.Flow.single_read source buf with
    | n -> loop (Cstruct.to_string buf ~len:n :: acc)
    | exception End_of_file -> (List.rev acc, None)
    | exception e -> (List.rev acc, Some e)
  in
  let chunks, failure = loop [] in
  {
    status = Http.Status.to_int resp.Http.Response.status;
    content_type = Http.Header.get resp.Http.Response.headers "content-type";
    chunks;
    failure;
  }

let with_server source f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match F.start ~sw ~net:(Eio.Stdenv.net env) source with
  | Error e -> Alcotest.failf "start: %a" F.pp_error e
  | Ok server ->
      f ~sw env server;
      Eio.Switch.fail sw Exit

let run_server source f = try with_server source f with Exit -> ()
let read path = In_channel.with_open_bin path In_channel.input_all

let ok_canon text =
  match Canon.of_string text with
  | Ok c -> c
  | Error _ -> Alcotest.fail "bad json"

type case = { dir : string; name : string }

let cases () =
  Sys.readdir fixtures_root |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun p ->
      Sys.readdir (Filename.concat fixtures_root p)
      |> Array.to_list |> List.sort String.compare
      |> List.map (fun c ->
          {
            dir = Filename.concat (Filename.concat fixtures_root p) c;
            name = p ^ "/" ^ c;
          }))

let file c n = Filename.concat c.dir n

(* Every recorded fixture comes back byte for byte, with its recorded status,
   content type and chunk boundaries, even when the request is re-serialised
   with another key order. *)
let test_every_fixture () =
  let all = cases () in
  Alcotest.(check bool) "fixtures present" true (List.length all >= 20);
  run_server (F.Fixtures fixtures_root) (fun ~sw env server ->
      List.iter
        (fun c ->
          let meta = ok_canon (read (file c "meta.json")) in
          let str k = Option.get (Canon.str_member k meta) in
          let reordered =
            (* Canon sorts keys; reverse top-level fields to change the order. *)
            match
              Canon.to_fields_opt (ok_canon (read (file c "request.json")))
            with
            | Some fields ->
                "{"
                ^ String.concat ","
                    (List.rev_map
                       (fun (k, v) ->
                         Printf.sprintf "%S:%s" k (Canon.to_string v))
                       fields)
                ^ "}"
            | None -> Alcotest.fail "request is not an object"
          in
          let r =
            post ~sw env ~query:(str "query") (F.base_url server) (str "path")
              reordered
          in
          Alcotest.(check int)
            (c.name ^ " status")
            (Option.get (Canon.int_member "status" meta))
            r.status;
          Alcotest.(check (option string))
            (c.name ^ " content-type")
            (Option.bind
               (Canon.member "response_headers" meta)
               (Canon.str_member "content-type"))
            r.content_type;
          Alcotest.(check string)
            (c.name ^ " body")
            (read (file c "response.raw"))
            (body_of r);
          Alcotest.(check bool)
            (c.name ^ " clean end") true (Option.is_none r.failure);
          let lengths =
            List.map
              (fun b -> Option.get (Canon.int_member "length" b))
              (Option.get
                 (Canon.to_list_opt (ok_canon (read (file c "chunks.json")))))
          in
          Alcotest.(check (list int))
            (c.name ^ " chunk boundaries")
            lengths
            (List.map String.length r.chunks))
        all)

let test_no_match () =
  let c = List.hd (cases ()) in
  let meta = ok_canon (read (file c "meta.json")) in
  let path = Option.get (Canon.str_member "path" meta) in
  let request = read (file c "request.json") in
  run_server (F.Fixtures fixtures_root) (fun ~sw env server ->
      let base = F.base_url server in
      let status ?query p b = (post ~sw env ?query base p b).status in
      Alcotest.(check int) "other path" 404 (status "/v1/nothing" request);
      Alcotest.(check int) "other body" 404 (status path {|{"model":"x"}|});
      Alcotest.(check int)
        "other query" 404
        (status ~query:"alt=json" path request);
      Alcotest.(check int) "not json" 400 (status path "not json");
      Alcotest.(check int)
        "still serves" 200
        (status
           ~query:(Option.value ~default:"" (Canon.str_member "query" meta))
           path request))

let test_query_selects () =
  let dir = "gemini_generate" in
  let body d =
    read
      (Filename.concat
         (Filename.concat fixtures_root dir)
         (d ^ "/request.json"))
  in
  let path = "/v1beta/models/gemini-3.8-flash:streamGenerateContent" in
  run_server (F.Fixtures fixtures_root) (fun ~sw env server ->
      let b = body "flash_stream_json" in
      let json = post ~sw env (F.base_url server) path b in
      let sse = post ~sw env ~query:"alt=sse" (F.base_url server) path b in
      Alcotest.(check bool)
        "json type" true
        (Option.fold ~none:false
           ~some:(String.starts_with ~prefix:"application/json")
           json.content_type);
      Alcotest.(check (option string))
        "sse type" (Some "text/event-stream") sse.content_type)

let sample_script =
  [
    F.
      {
        status = 200;
        headers = [ ("content-type", "text/plain") ];
        chunks = [ "one"; "two" ];
        ending = Complete;
      };
    F.
      {
        status = 429;
        headers = [ ("retry-after", "1"); ("content-type", "application/json") ];
        chunks = [ {|{"error":"slow down"}|} ];
        ending = Complete;
      };
    F.
      {
        status = 200;
        headers = [ ("content-type", "text/event-stream") ];
        chunks = [ "data: a\n\n"; "data: b\n\n"; "data: c" ];
        ending = Drop;
      };
  ]

let test_script () =
  run_server (F.Script sample_script) (fun ~sw env server ->
      let base = F.base_url server in
      let first = post ~sw env base "/x" "{}" in
      Alcotest.(check int) "200" 200 first.status;
      Alcotest.(check (list string)) "chunks kept" [ "one"; "two" ] first.chunks;
      let second = post ~sw env base "/x" "{}" in
      Alcotest.(check int) "429" 429 second.status;
      Alcotest.(check string)
        "429 body" {|{"error":"slow down"}|} (body_of second);
      let third = post ~sw env base "/x" "{}" in
      Alcotest.(check int) "truncated status" 200 third.status;
      Alcotest.(check string)
        "bytes before the drop" "data: a\n\ndata: b\n\ndata: c" (body_of third);
      let fourth = post ~sw env base "/x" "{}" in
      Alcotest.(check int) "script exhausted" 500 fourth.status)

(* The cohttp client treats EOF as the end of a chunked body, so check the raw
   bytes: the connection must close without the terminating chunk. *)
let test_drop_is_truncation () =
  let reply =
    F.{ status = 200; headers = []; chunks = [ "abc"; "de" ]; ending = Drop }
  in
  run_server (F.Script [ reply ]) (fun ~sw:_ env server ->
      let net = Eio.Stdenv.net env in
      let raw =
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 3.0 (fun () ->
            Eio.Net.with_tcp_connect ~host:"127.0.0.1"
              ~service:(string_of_int (F.port server))
              net
              (fun flow ->
                Eio.Flow.copy_string
                  "POST /x HTTP/1.1\r\nhost: x\r\ncontent-length: 2\r\n\r\n{}"
                  flow;
                Eio.Buf_read.take_all
                  (Eio.Buf_read.of_flow ~max_size:max_int flow)))
      in
      Alcotest.(check bool)
        "chunks sent" true
        (String.ends_with ~suffix:"3\r\nabc\r\n2\r\nde\r\n" raw);
      Alcotest.(check bool)
        "no terminating chunk" false
        (String.ends_with ~suffix:"0\r\n\r\n" raw))

let test_request_log () =
  run_server
    (F.Script
       [
         F.{ status = 200; headers = []; chunks = [ "ok" ]; ending = Complete };
       ])
    (fun ~sw env server ->
      let _ =
        post ~sw env
          ~headers:[ ("X-Probe", "yes") ]
          ~query:"a=1&b=2" (F.base_url server) "/v1/echo" {|{"k":1}|}
      in
      match F.requests server with
      | [ r ] ->
          Alcotest.(check string) "method" "POST" r.meth;
          Alcotest.(check string) "path" "/v1/echo" r.path;
          Alcotest.(check string) "query" "a=1&b=2" r.query;
          Alcotest.(check string) "body" {|{"k":1}|} r.body;
          Alcotest.(check (option string))
            "header lower-cased" (Some "yes")
            (List.assoc_opt "x-probe" r.headers)
      | l -> Alcotest.failf "expected one request, got %d" (List.length l))

let start_error source =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  F.start ~sw ~net:(Eio.Stdenv.net env) source |> Result.map ignore

let test_bad_fixtures () =
  (match start_error (F.Fixtures "/nonexistent-fixtures-dir") with
  | Error (F.Unreadable _) -> ()
  | Ok () | Error (F.Invalid_fixture _) -> Alcotest.fail "expected Unreadable");
  let root = Filename.temp_dir "fake_upstream" "" in
  let case = Filename.concat (Filename.concat root "p") "c" in
  Sys.mkdir (Filename.concat root "p") 0o755;
  Sys.mkdir case 0o755;
  let write n s =
    Out_channel.with_open_bin (Filename.concat case n) (fun oc ->
        output_string oc s)
  in
  write "meta.json" {|{"method":"POST","path":"/x","query":"","status":200}|};
  write "request.json" "{}";
  write "response.raw" "hello";
  write "chunks.json" {|[{"offset":0,"length":3}]|};
  let result = start_error (F.Fixtures root) in
  List.iter
    (fun n -> Sys.remove (Filename.concat case n))
    [ "meta.json"; "request.json"; "response.raw"; "chunks.json" ];
  Sys.rmdir case;
  Sys.rmdir (Filename.concat root "p");
  Sys.rmdir root;
  match result with
  | Error (F.Invalid_fixture _) -> ()
  | Ok () | Error (F.Unreadable _) -> Alcotest.fail "expected Invalid_fixture"

let () =
  Alcotest.run "fake_upstream"
    [
      ( "fixtures",
        [
          Alcotest.test_case "every fixture replays" `Quick test_every_fixture;
          Alcotest.test_case "no match" `Quick test_no_match;
          Alcotest.test_case "query selects" `Quick test_query_selects;
          Alcotest.test_case "bad fixtures" `Quick test_bad_fixtures;
        ] );
      ( "script",
        [
          Alcotest.test_case "sequence, 429, truncation" `Quick test_script;
          Alcotest.test_case "drop truncates the stream" `Quick
            test_drop_is_truncation;
          Alcotest.test_case "request log" `Quick test_request_log;
        ] );
    ]
