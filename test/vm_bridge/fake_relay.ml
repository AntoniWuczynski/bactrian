(* Stands in for vm/relay.c in the bridge's tests. The first argument picks
   what it does:
   - echo: sends back each chunk of channel 0, and exits 7 at its end;
   - connect PORT: opens channel 1 to PORT, sends "ping" and its end, and
     writes what came back on channel 0 before exiting 0;
   - log: one log line, then exit 0;
   - heartbeat: waits for the bridge's heartbeat, says "beat" on channel 0
     and exits 0;
   - bad: a frame of an unknown kind;
   - silent: ends without an exit frame. *)

module F = Bactrian_core.Vm_frame

let send frames =
  List.iter
    (fun f ->
      let s = F.encode f in
      ignore (Unix.write_substring Unix.stdout s 0 (String.length s) : int))
    frames

(* The frames that arrive until [stop] says enough. *)
let receive stop =
  let d = F.decoder () in
  let buf = Bytes.create 65536 in
  let rec go acc =
    if stop acc then List.rev acc
    else
      match Unix.read Unix.stdin buf 0 (Bytes.length buf) with
      | 0 -> List.rev acc
      | n -> (
          match F.feed d (Bytes.sub_string buf 0 n) with
          | Ok fs -> go (List.rev_append fs acc)
          | Error _ -> exit 3)
  in
  go []

let ended channel =
  List.exists (fun (f : F.t) ->
      f.channel = channel
      &&
      match f.kind with
      | Eof | Close -> true
      | Data | Open | Exit | Log | Heartbeat -> false)

let payloads channel frames =
  String.concat ""
    (List.filter_map
       (fun (f : F.t) ->
         match f.kind with
         | Data when f.channel = channel -> Some f.payload
         | Data | Eof | Open | Close | Exit | Log | Heartbeat -> None)
       frames)

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [ "echo" ] ->
      let frames = receive (ended 0) in
      send
        (F.data ~channel:0 (payloads 0 frames) @ [ F.eof ~channel:0; F.exit 7 ])
  | [ "connect"; port ] ->
      send
        ([ F.open_ ~channel:1 ~port:(int_of_string port) ]
        @ F.data ~channel:1 "ping"
        @ [ F.eof ~channel:1 ]);
      let frames = receive (ended 1) in
      send (F.data ~channel:0 (payloads 1 frames) @ [ F.exit 0 ])
  | [ "heartbeat" ] ->
      let (_ : F.t list) =
        receive
          (List.exists (fun (f : F.t) ->
               match f.kind with
               | Heartbeat -> true
               | Data | Eof | Open | Close | Exit | Log -> false))
      in
      send (F.data ~channel:0 "beat" @ [ F.exit 0 ])
  | [ "log" ] ->
      send
        [
          F.log
            {|{"kind":"relay_failed","port":1,"socket":"relay","reason":"r"}|};
          F.exit 0;
        ]
  | [ "bad" ] -> print_string "\000\000\000\000\009\000\000\000\000"
  | [ "silent" ] -> ()
  | _ -> exit 2
