module Egress_proxy = Bactrian_runtime.Egress_proxy

let failure = function
  | Tls_eio.Tls_failure f ->
      Some ("TLS failure: " ^ Tls.Engine.string_of_failure f)
  | Tls_eio.Tls_alert a ->
      Some ("TLS alert from the agent: " ^ Tls.Packet.alert_type_to_string a)
  | _ -> None

let terminate ca ~sim_now ~host flow =
  let ( let* ) = Result.bind in
  let* chain = Ca.leaf ca ~now:(Ptime_clock.now ()) ~sim_now ~host in
  let* config =
    Result.map_error
      (fun (`Msg m) -> "TLS configuration: " ^ m)
      (Tls.Config.server ~certificates:(`Single chain)
         ~alpn_protocols:[ "http/1.1" ] ())
  in
  match Tls_eio.server_of_flow config flow with
  | tls -> Ok (tls :> Bactrian_runtime.Upstream.flow)
  | exception End_of_file ->
      Error "the agent closed the connection during the TLS handshake"
  | exception ((Tls_eio.Tls_failure _ | Tls_eio.Tls_alert _) as e) ->
      Error (Option.value (failure e) ~default:"TLS failure")

(* A SIM_NOW that does not parse, or is before the epoch, leaves the agent's
   clock alone (Agent_clock), so the real time is the only one. *)
let tls ca ~sim_now : Egress_proxy.tls =
  let now = Ptime_clock.now () in
  let sim_now =
    match Bactrian_core.Rfc3339.epoch sim_now with
    | Some e when e >= 0 ->
        Option.value (Ptime.of_span (Ptime.Span.of_int_s e)) ~default:now
    | Some _ | None -> now
  in
  { terminate = terminate ca ~sim_now; failure }

let interception ca ~roots : Egress_proxy.interception =
  {
    tls = tls ca;
    bundle = X509.Certificate.encode_pem_multiple roots ^ Ca.certificate_pem ca;
  }
