let ( let* ) = Result.bind
let connector process_mgr = Bactrian_tls.Tls_connector.system ~process_mgr
let ca_dir ~root = Filename.concat root "ca"

let ca ~root =
  Bactrian_tls.Ca.load_or_create ~dir:(ca_dir ~root) ~now:(Ptime_clock.now ())

let describe_ca ca =
  let module Ca = Bactrian_tls.Ca in
  let (y, m, d), _ = Ptime.to_date_time (Ca.not_after ca) in
  Printf.sprintf "the project CA is %s, valid until %04d-%02d-%02d, SHA-256 %s"
    (Ca.certificate_path ca) y m d (Ca.fingerprint ca)

let interception process_mgr ~root () =
  let* ca, _ = ca ~root in
  (* Without the system's roots the bundle holds the CA alone, which is all an
     intercepted client needs. *)
  let roots =
    match Bactrian_tls.Trust.load ~process_mgr with
    | Ok t -> t.system
    | Error _ -> []
  in
  Ok (Bactrian_tls.Egress_tls.interception ca ~roots)
