(* Spike: can cohttp-eio proxy Server-Sent Events chunk by chunk? *)

type item = Data of string | End | Abort

exception Upstream_aborted

(* An Eio source fed from a stream; leftovers are kept when [dst] is small. *)
module Stream_source = struct
  type t = {
    q : item Eio.Stream.t;
    mutable pending : string;
    mutable pos : int;
  }

  let rec single_read t dst =
    if t.pos < String.length t.pending then begin
      let n = min (Cstruct.length dst) (String.length t.pending - t.pos) in
      Cstruct.blit_from_string t.pending t.pos dst 0 n;
      t.pos <- t.pos + n;
      n
    end
    else
      match Eio.Stream.take t.q with
      | Data s ->
          t.pending <- s;
          t.pos <- 0;
          single_read t dst
      | End -> raise End_of_file
      | Abort -> raise Upstream_aborted

  let read_methods = []
end

let source_of_stream q : Eio.Flow.source_ty Eio.Resource.t =
  let ops = Eio.Flow.Pi.source (module Stream_source) in
  Eio.Resource.T ({ Stream_source.q; pending = ""; pos = 0 }, ops)

let serve ~sw net ~callback =
  let sock =
    Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr sock with
    | `Tcp (_, p) -> p
    | `Unix _ -> failwith "unexpected unix address"
  in
  let server = Cohttp_eio.Server.make ~callback () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Cohttp_eio.Server.run sock server ~on_error:(fun _ -> ()));
  port

let sse_headers = Http.Header.of_list [ ("content-type", "text/event-stream") ]

(* Upstream behaviour for one request. *)
type script = { events : string list; gap : float; ending : item }

let upstream ~sw ~env script =
  let clock = Eio.Stdenv.clock env in
  serve ~sw (Eio.Stdenv.net env) ~callback:(fun _ _ _body ->
      let q = Eio.Stream.create max_int in
      Eio.Fiber.fork ~sw (fun () ->
          List.iter
            (fun e ->
              Eio.Stream.add q (Data e);
              Eio.Time.sleep clock script.gap)
            script.events;
          Eio.Stream.add q script.ending);
      Cohttp_eio.Server.respond ~headers:sse_headers ~status:`OK
        ~body:(source_of_stream q) ())

let proxy ~sw ~env ~upstream_port =
  let client = Cohttp_eio.Client.make ~https:None (Eio.Stdenv.net env) in
  serve ~sw (Eio.Stdenv.net env) ~callback:(fun _ _ _body ->
      let q = Eio.Stream.create max_int in
      let uri =
        Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/" upstream_port)
      in
      let _resp, body = Cohttp_eio.Client.get client ~sw uri in
      Eio.Fiber.fork ~sw (fun () ->
          let buf = Cstruct.create 65536 in
          let rec loop () =
            match Eio.Flow.single_read body buf with
            | n ->
                Eio.Stream.add q (Data (Cstruct.to_string buf ~len:n));
                loop ()
            | exception End_of_file -> Eio.Stream.add q End
            | exception _ -> Eio.Stream.add q End
          in
          loop ());
      Cohttp_eio.Server.respond ~headers:sse_headers ~status:`OK
        ~body:(source_of_stream q) ())

(* Client: returns the full bytes and arrival time of each complete event. *)
let read_events ~sw ~env ~port =
  let clock = Eio.Stdenv.clock env in
  let client = Cohttp_eio.Client.make ~https:None (Eio.Stdenv.net env) in
  let uri = Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/" port) in
  let _resp, body = Cohttp_eio.Client.get client ~sw uri in
  let buf = Cstruct.create 65536 in
  let acc = Buffer.create 1024 in
  let times = ref [] in
  let scanned = ref 0 in
  let rec loop () =
    match Eio.Flow.single_read body buf with
    | n ->
        let now = Eio.Time.now clock in
        Buffer.add_string acc (Cstruct.to_string buf ~len:n);
        let s = Buffer.contents acc in
        let i = ref (max 0 (!scanned - 1)) in
        while !i + 1 < String.length s do
          if s.[!i] = '\n' && s.[!i + 1] = '\n' then begin
            times := now :: !times;
            i := !i + 2
          end
          else incr i
        done;
        scanned := !i;
        loop ()
    | exception End_of_file -> ()
  in
  loop ();
  (Buffer.contents acc, List.rev !times)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.printf "FAIL %s\n%!" name
  end

let run_case ~env ~name ~timeout script ~verify =
  Eio.Switch.run @@ fun sw ->
  let up = upstream ~sw ~env script in
  let px = proxy ~sw ~env ~upstream_port:up in
  let result =
    Eio.Time.with_timeout (Eio.Stdenv.clock env) timeout (fun () ->
        Ok (read_events ~sw ~env ~port:px))
  in
  (match result with
  | Ok (bytes, times) -> verify ~bytes ~times
  | Error `Timeout -> check (name ^ ": timed out") false);
  Printf.printf "case %s done\n%!" name;
  Eio.Switch.fail sw Exit

let gaps times =
  let rec go = function a :: (b :: _ as r) -> (b -. a) :: go r | _ -> [] in
  go times

let () =
  Eio_main.run @@ fun env ->
  let guard f = try f () with Exit -> () in
  guard (fun () ->
      let events =
        List.init 5 (fun i -> Printf.sprintf "data: event-%d\n\n" (i + 1))
      in
      run_case ~env ~name:"timing" ~timeout:5.0
        { events; gap = 0.1; ending = End } ~verify:(fun ~bytes ~times ->
          let g = gaps times in
          Printf.printf "timing gaps (ms): %s\n%!"
            (String.concat " "
               (List.map (fun x -> Printf.sprintf "%.0f" (x *. 1000.)) g));
          check "5 events" (List.length times = 5);
          check "bytes equal" (String.equal bytes (String.concat "" events));
          check "gaps >= 60ms" (List.for_all (fun x -> x >= 0.06) g)));
  guard (fun () ->
      let ev = "data: " ^ String.make (200 * 1024) 'x' ^ "\n\n" in
      run_case ~env ~name:"large" ~timeout:5.0
        { events = [ ev ]; gap = 0.0; ending = End }
        ~verify:(fun ~bytes ~times ->
          Printf.printf "large: %d bytes, %d events\n%!" (String.length bytes)
            (List.length times);
          check "large bytes equal" (String.equal bytes ev);
          check "large 1 event" (List.length times = 1)));
  guard (fun () ->
      let events = [ "data: event-1\n\n"; "data: event-2\n\n"; "data: eve" ] in
      let t0 = Eio.Time.now (Eio.Stdenv.clock env) in
      run_case ~env ~name:"abort" ~timeout:2.0
        { events; gap = 0.1; ending = Abort } ~verify:(fun ~bytes ~times ->
          let dt = Eio.Time.now (Eio.Stdenv.clock env) -. t0 in
          Printf.printf
            "abort: %d complete events, %d bytes, ended after %.0f ms\n%!"
            (List.length times) (String.length bytes) (dt *. 1000.);
          check "abort 2 events" (List.length times = 2);
          check "abort prefix"
            (String.length bytes >= 30
            && String.equal (String.sub bytes 0 30)
                 "data: event-1\n\ndata: event-2\n\n")));
  if !failures > 0 then exit 1 else print_endline "ALL PASS"
