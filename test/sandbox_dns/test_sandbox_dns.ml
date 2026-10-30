module Dns_server = Bactrian_runtime.Sandbox_dns
module Buf_read = Eio.Buf_read

type ctx = { env : Eio_unix.Stdenv.base; sw : Eio.Switch.t }

let with_ctx f =
  Eio_main.run @@ fun env ->
  (* Only a guard against a hang: nothing waits for this to expire. *)
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 60. @@ fun () ->
  Eio.Switch.run @@ fun sw -> f { env; sw }

let loopback = Eio.Net.Ipaddr.V4.loopback

let connect ctx port =
  Eio.Net.connect ~sw:ctx.sw (Eio.Stdenv.net ctx.env) (`Tcp (loopback, port))

let reader flow = Buf_read.of_flow ~max_size:max_int flow
let half_close flow = Eio.Flow.shutdown flow `Send

(* The server *)

let dns_query ?(flags = "\001\000") labels =
  "\012\034" ^ flags ^ "\000\001\000\000\000\000\000\000"
  ^ String.concat ""
      (List.map
         (fun l -> String.make 1 (Char.chr (String.length l)) ^ l)
         labels)
  ^ "\000\000\028\000\001"

let with_dns f =
  with_ctx @@ fun ctx ->
  let lines = ref [] and ports = ref [] in
  let net = Eio.Stdenv.net ctx.env in
  (match
     Dns_server.serve ~sw:ctx.sw ~net
       ~on_bound:(fun t p -> ports := (t, p) :: !ports)
       ~record:(fun l -> lines := l :: !lines)
       [ (loopback, 0) ]
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail (Dns_server.describe_error e));
  let port t =
    match List.assoc_opt t !ports with
    | Some p -> p
    | None -> Alcotest.fail "not bound"
  in
  f ctx ~port ~lines:(fun () -> List.rev !lines)

let refused_reply query reply =
  Alcotest.(check string)
    "same id" (String.sub query 0 2) (String.sub reply 0 2);
  Alcotest.(check int) "REFUSED" 5 (Char.code reply.[3] land 0x0f);
  Alcotest.(check int) "a response" 0x80 (Char.code reply.[2] land 0x80)

let dns_over_udp () =
  with_dns @@ fun ctx ~port ~lines ->
  let sock =
    Eio.Net.datagram_socket ~sw:ctx.sw (Eio.Stdenv.net ctx.env)
      (`Udp (loopback, 0))
  in
  let ask q =
    Eio.Net.send sock ~dst:(`Udp (loopback, port `Udp)) [ Cstruct.of_string q ]
  in
  let q = dns_query [ "example"; "com" ] in
  ask q;
  let buf = Cstruct.create 512 in
  let _, n = Eio.Net.recv sock buf in
  refused_reply q (Cstruct.to_string ~len:n buf);
  (* Nothing comes back for a response, and the next query is still served. *)
  ask (dns_query ~flags:"\129\000" [ "x" ]);
  ask "\001\002";
  ask q;
  let _, n = Eio.Net.recv sock buf in
  refused_reply q (Cstruct.to_string ~len:n buf);
  Alcotest.(check (list string))
    "one line per message"
    [
      {|{"kind":"dns","transport":"udp","name":"example.com","type":"AAAA"}|};
      {|{"kind":"dns","transport":"udp","error":"the message is a response, not a query"}|};
      {|{"kind":"dns","transport":"udp","error":"the message ends inside its header or question"}|};
      {|{"kind":"dns","transport":"udp","name":"example.com","type":"AAAA"}|};
    ]
    (lines ())

let dns_over_tcp () =
  with_dns @@ fun ctx ~port ~lines ->
  let flow = connect ctx (port `Tcp) in
  let prefixed q =
    let n = String.length q in
    String.make 1 (Char.chr (n lsr 8))
    ^ String.make 1 (Char.chr (n land 0xff))
    ^ q
  in
  let q1 = dns_query [ "a"; "test" ] and q2 = dns_query [ "b" ] in
  Eio.Flow.copy_string (prefixed q1 ^ prefixed q2) flow;
  let r = reader flow in
  let answer () =
    let len = Buf_read.take 2 r in
    Buf_read.take ((Char.code len.[0] lsl 8) lor Char.code len.[1]) r
  in
  refused_reply q1 (answer ());
  refused_reply q2 (answer ());
  half_close flow;
  Alcotest.(check bool)
    "closed after the last query" true
    (Buf_read.at_end_of_input r);
  Alcotest.(check (list string))
    "both recorded"
    [
      {|{"kind":"dns","transport":"tcp","name":"a.test","type":"AAAA"}|};
      {|{"kind":"dns","transport":"tcp","name":"b","type":"AAAA"}|};
    ]
    (lines ())

let dns_lines_are_read_back () =
  List.iter
    (fun (transport, query) ->
      let line =
        Dns_server.line ~transport (Bactrian_core.Dns_message.parse query)
      in
      match Bactrian_core.Sandbox_log.of_relay_line line with
      | Ok v -> Alcotest.(check string) line "dns" v.kind
      | Error m -> Alcotest.failf "%s: %s" line m)
    [ (`Udp, dns_query [ "a\"b"; "c" ]); (`Tcp, "\000") ]

(* 192.0.2.1 is a documentation address, assigned to no interface. *)
let dns_bind_fails () =
  with_ctx @@ fun ctx ->
  match
    Dns_server.serve ~sw:ctx.sw ~net:(Eio.Stdenv.net ctx.env) ~record:ignore
      [ (Eio.Net.Ipaddr.of_raw "\192\000\002\001", 0) ]
  with
  | Error { address; _ } ->
      Alcotest.(check string) "address" "192.0.2.1:0" address
  | Ok () -> Alcotest.fail "bound an address no interface has"

(* The resolvers it does not answer *)

let nameservers () =
  let check name expected text =
    Alcotest.(check (list string))
      name expected
      (Dns_server.unanswered_nameservers text)
  in
  check "none named" [] "search example.com\noptions edns0\n";
  check "the forwarder's addresses" []
    "nameserver 127.0.0.53\nnameserver\t127.0.0.1  # local\n";
  check "others, in order"
    [ "192.168.1.1"; "::1"; "10.0.0.2" ]
    "# nameserver 9.9.9.9\n\
     nameserver 192.168.1.1\n\
     nameserver 127.0.0.53\n\
     nameserver ::1\n\
    \  nameserver 10.0.0.2\n\
     nameservers 1.1.1.1\n"

let resolv_conf () =
  let dir = Filename.temp_dir "bactrian-dns" "" in
  let file = Filename.concat dir "resolv.conf"
  and link = Filename.concat dir "link" in
  Fun.protect ~finally:(fun () ->
      List.iter Sys.remove [ link; file ];
      Unix.rmdir dir)
  @@ fun () ->
  Out_channel.with_open_bin file (fun oc ->
      output_string oc "nameserver 10.0.0.1\n");
  Unix.symlink file link;
  let unrecorded path = Dns_server.unrecorded_resolvers ~path () in
  Alcotest.(check (list string))
    "through a link" [ "10.0.0.1" ] (unrecorded link);
  Alcotest.(check (list string))
    "missing" []
    (unrecorded (Filename.concat dir "missing"))

let () =
  Alcotest.run "sandbox_dns"
    [
      ( "server",
        [
          Alcotest.test_case "over UDP" `Quick dns_over_udp;
          Alcotest.test_case "over TCP" `Quick dns_over_tcp;
          Alcotest.test_case "lines read back" `Quick dns_lines_are_read_back;
          Alcotest.test_case "an address it cannot bind" `Quick dns_bind_fails;
        ] );
      ( "resolvers",
        [
          Alcotest.test_case "nameserver lines" `Quick nameservers;
          Alcotest.test_case "resolv.conf" `Quick resolv_conf;
        ] );
    ]
