type ca = { signer : X509.Private_key.t; cert : X509.Certificate.t }

let dn cn =
  [
    X509.Distinguished_name.(
      Relative_distinguished_name.singleton (CN (Common_name.v cn)));
  ]

let ok = function Ok x -> x | Error (`Msg m) -> failwith ("fake_https: " ^ m)

let span days =
  let now = Ptime_clock.now () in
  let shift d =
    Option.get (Ptime.add_span now (Ptime.Span.of_int_s (d * 86_400)))
  in
  (shift (-1), shift days)

let sign ~issuer ~issuer_dn ~subject_key ~subject ~extensions ~days =
  let csr = ok (X509.Signing_request.create subject subject_key) in
  let valid_from, valid_until = span days in
  match
    X509.Signing_request.sign csr ~valid_from ~valid_until ~extensions issuer
      issuer_dn
  with
  | Ok cert -> cert
  | Error e ->
      failwith
        (Format.asprintf "fake_https: %a" X509.Validation.pp_signature_error e)

let ca () =
  let signer = X509.Private_key.generate `P256 in
  let extensions =
    X509.Extension.(
      add Basic_constraints
        (true, (true, None))
        (singleton Key_usage (true, [ `Key_cert_sign; `CRL_sign ])))
  in
  let name = dn "bactrian test CA" in
  let cert =
    sign ~issuer:signer ~issuer_dn:name ~subject_key:signer ~subject:name
      ~extensions ~days:365
  in
  { signer; cert }

let ca_pem ca = X509.Certificate.encode_pem ca.cert

let authenticator ca =
  X509.Authenticator.chain_of_trust
    ~time:(fun () -> Some (Ptime_clock.now ()))
    [ ca.cert ]

type name = Dns of string | Ip of string

let leaf ca names =
  let signer = X509.Private_key.generate `P256 in
  let dns = List.filter_map (function Dns d -> Some d | Ip _ -> None) names
  and ips =
    List.filter_map
      (function
        | Ip a -> Some (Ipaddr.to_octets (Ipaddr.of_string_exn a))
        | Dns _ -> None)
      names
  in
  let san = X509.General_name.(add DNS dns (singleton IP ips)) in
  let extensions =
    X509.Extension.(
      add Subject_alt_name (false, san)
        (add Basic_constraints
           (true, (false, None))
           (add Key_usage
              (true, [ `Digital_signature ])
              (singleton Ext_key_usage (false, [ `Server_auth ])))))
  in
  let cn = match names with Dns d :: _ | Ip d :: _ -> d | [] -> "none" in
  let cert =
    sign ~issuer:ca.signer
      ~issuer_dn:(X509.Certificate.subject ca.cert)
      ~subject_key:signer ~subject:(dn cn) ~extensions ~days:7
  in
  ([ cert; ca.cert ], signer)

type t = { port : int }

let rec pieces size s =
  if String.length s <= size then [ s ]
  else
    String.sub s 0 size
    :: pieces size (String.sub s size (String.length s - size))

let relay ~net ~record_size ~close_notify ~config ~backend socket =
  match Tls_eio.server_of_flow config socket with
  | exception (End_of_file | Tls_eio.Tls_failure _ | Tls_eio.Tls_alert _) -> ()
  | exception Eio.Io _ -> ()
  | tls ->
      Eio.Switch.run @@ fun sw ->
      let up =
        Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, backend))
      in
      (* The relay ends when the backend does: the client's direction is then
         cancelled, so the socket really closes when the handler returns. *)
      Eio.Fiber.first
        (fun () ->
          (try
             Eio.Flow.copy tls up;
             Eio.Flow.shutdown up `Send
           with
           | End_of_file | Eio.Io _ | Tls_eio.Tls_failure _
           | Tls_eio.Tls_alert _
           ->
             ());
          Eio.Fiber.await_cancel ())
        (fun () ->
          let buf = Cstruct.create 65536 in
          let rec loop () =
            match Eio.Flow.single_read up buf with
            | n ->
                let s = Cstruct.to_string buf ~off:0 ~len:n in
                List.iter
                  (fun p -> Eio.Flow.copy_string p tls)
                  (match record_size with
                  | Some k -> pieces k s
                  | None -> [ s ]);
                loop ()
            | exception (End_of_file | Eio.Io _) -> ()
          in
          (try loop () with Eio.Io _ -> ());
          try if close_notify then Eio.Flow.shutdown tls `Send
          with Eio.Io _ -> ())

let start ~sw ~net ?record_size ?(close_notify = true) ~chain ~backend () =
  let config =
    ok
      (Tls.Config.server ~certificates:(`Single chain)
         ~alpn_protocols:[ "http/1.1" ] ())
  in
  let listener =
    Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr listener with
    | `Tcp (_, p) -> p
    | `Unix _ -> failwith "fake_https: not a TCP listener"
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server listener
        ~on_error:(fun _ -> ())
        (fun socket _ ->
          relay ~net ~record_size ~close_notify ~config ~backend socket));
  { port }

let port t = t.port
