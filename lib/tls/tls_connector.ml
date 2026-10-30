module Upstream = Bactrian_runtime.Upstream

let ( let* ) = Result.bind

type peer = Host of [ `host ] Domain_name.t | Ip of Ipaddr.t

(* Uri gives an IPv6 literal without its brackets. *)
let peer url =
  match Uri.host url with
  | None -> Error "the URL has no host"
  | Some host -> (
      match Ipaddr.of_string host with
      | Ok ip -> Ok (Ip ip)
      | Error (`Msg _) -> (
          match Result.bind (Domain_name.of_string host) Domain_name.host with
          | Ok name -> Ok (Host name)
          | Error (`Msg reason) ->
              Error (Printf.sprintf "%S is not a host name: %s" host reason)))

let failure = function
  | Tls_eio.Tls_failure f ->
      Some ("TLS failure: " ^ Tls.Engine.string_of_failure f)
  | Tls_eio.Tls_alert a ->
      Some ("TLS alert from the server: " ^ Tls.Packet.alert_type_to_string a)
  | _ -> None

let handshake config peer flow =
  match
    match peer with
    | Host host -> Tls_eio.client_of_flow config ~host flow
    | Ip ip -> Tls_eio.client_of_flow config ~ip flow
  with
  | tls -> Ok (tls :> Upstream.flow)
  | exception End_of_file ->
      Error "the server closed the connection during the TLS handshake"
  | exception (Tls_eio.Tls_failure _ as e) ->
      Error (Option.value (failure e) ~default:"TLS failure")
  | exception (Tls_eio.Tls_alert _ as e) ->
      Error (Option.value (failure e) ~default:"TLS alert")

let make ~trust : Upstream.https =
  let wrap url flow =
    let* authenticator, _ = trust () in
    let* peer = peer url in
    let* config =
      Result.map_error
        (fun (`Msg m) -> "TLS configuration: " ^ m)
        (Tls.Config.client ~authenticator ~alpn_protocols:[ "http/1.1" ] ())
    in
    handshake config peer flow
  in
  { ready = (fun () -> Result.map snd (trust ())); wrap; failure }

let system ~process_mgr =
  let roots =
    Eio.Lazy.from_fun ~cancel:`Restart (fun () -> Trust.load ~process_mgr)
  in
  let trust () =
    Result.map
      (fun ({ system; extra } : Trust.t) ->
        let roots = system @ extra in
        ( X509.Authenticator.chain_of_trust
            ~time:(fun () -> Some (Ptime_clock.now ()))
            roots,
          {
            Upstream.roots = List.length roots;
            extra =
              (match extra with
              | [] -> None
              | _ :: _ ->
                  Some
                    {
                      variable = Trust.extra_variable;
                      count = List.length extra;
                    });
          } ))
      (Eio.Lazy.force roots)
  in
  Upstream.of_https (make ~trust)
