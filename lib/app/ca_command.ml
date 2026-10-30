module Ca = Bactrian_tls.Ca

type action = Path | Rotate

type t = {
  certificate : string;
  not_after : string;
  sha256 : string;
  created : bool;
}

let summary ca ~created =
  {
    certificate = Ca.certificate_path ca;
    not_after = Ptime.to_rfc3339 ~tz_offset_s:0 (Ca.not_after ca);
    sha256 = Ca.fingerprint ca;
    created;
  }

let run ~root action =
  Mirage_crypto_rng_unix.use_default ();
  Result.map_error
    (fun m -> Command_error.Environment m)
    (match action with
    | Path ->
        Result.map
          (fun (ca, (origin : Ca.origin)) ->
            summary ca
              ~created:(match origin with Created -> true | Loaded -> false))
          (Features.ca ~root)
    | Rotate ->
        Result.map (summary ~created:true)
          (Ca.rotate ~dir:(Features.ca_dir ~root) ~now:(Ptime_clock.now ())))

let to_json t =
  `Assoc
    [
      ("certificate", `String t.certificate);
      ("not_after", `String t.not_after);
      ("sha256", `String t.sha256);
      ("created", `Bool t.created);
    ]

let render_text action t =
  match action with
  | Path -> t.certificate ^ "\n"
  | Rotate ->
      Printf.sprintf "rotated: %s, valid until %s, SHA-256 %s\n" t.certificate
        t.not_after t.sha256
