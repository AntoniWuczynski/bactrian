let ( let* ) = Result.bind
let key_file = "ca.key"
let certificate_file = "ca.pem"

module Hosts = Hashtbl.Make (String)

type t = {
  signer : X509.Private_key.t;
  cert : X509.Certificate.t;
  path : string;
  leaves : (Tls.Config.certchain * Ptime.t * Ptime.t) Hosts.t;
  lock : Eio.Mutex.t;
}

type origin = Loaded | Created

let msg r = Result.map_error (fun (`Msg m) -> m) r

let days d =
  match Ptime.Span.of_float_s (float_of_int d *. 86_400.) with
  | Some s -> s
  | None -> assert false (* a few days fit in a span *)

let shift now d =
  match Ptime.add_span now (days d) with
  | Some t -> t
  | None -> assert false (* now plus or minus a year is a valid time *)

let dn cn =
  [
    X509.Distinguished_name.(
      Relative_distinguished_name.singleton (CN (Common_name.v cn)));
  ]

let sign ~issuer ~issuer_dn ~subject_key ~subject ~extensions ~valid_from
    ~valid_until =
  let* csr = msg (X509.Signing_request.create subject subject_key) in
  Result.map_error
    (Format.asprintf "%a" X509.Validation.pp_signature_error)
    (X509.Signing_request.sign csr ~valid_from ~valid_until ~extensions issuer
       issuer_dn)

let mint now =
  let signer = X509.Private_key.generate `P256 in
  let name = dn "bactrian project CA" in
  let extensions =
    X509.Extension.(
      add Basic_constraints
        (true, (true, Some 0))
        (add Key_usage
           (true, [ `Key_cert_sign; `CRL_sign ])
           (singleton Subject_key_id
              (false, X509.Public_key.id (X509.Private_key.public signer)))))
  in
  let* cert =
    sign ~issuer:signer ~issuer_dn:name ~subject_key:signer ~subject:name
      ~extensions ~valid_from:Ptime.epoch ~valid_until:(shift now 365)
  in
  Ok (signer, cert)

(* Files *)

let me = Unix.getuid ()

let private_to_us what path =
  match Unix.stat path with
  | exception Unix.Unix_error (e, _, _) ->
      Error (Printf.sprintf "%s: %s" path (Unix.error_message e))
  | st when st.st_uid <> me ->
      Error (Printf.sprintf "%s %s belongs to another user" what path)
  | st when st.st_perm land 0o077 <> 0 ->
      Error
        (Printf.sprintf
           "%s %s can be used by other users (mode %o): run chmod go= on it"
           what path st.st_perm)
  | _ -> Ok ()

let read path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> Ok text
  | exception Sys_error reason -> Error reason

let unix_error path f =
  match f () with
  | v -> Ok v
  | exception Sys_error reason -> Error reason
  | exception Unix.Unix_error (e, _, _) ->
      Error (Printf.sprintf "%s: %s" path (Unix.error_message e))

(* Written beside the target, to be renamed over it, so a reader never sees half
   a file. *)
let stage ~perm path text =
  let temp = path ^ ".new" in
  unix_error path (fun () ->
      (try Sys.remove temp with Sys_error _ -> ());
      Out_channel.with_open_gen
        [ Open_wronly; Open_creat; Open_excl; Open_binary ] perm temp (fun oc ->
          output_string oc text);
      Unix.chmod temp perm;
      temp)

let install ~temp path = unix_error path (fun () -> Unix.rename temp path)

(* Absolute, so that what bactrian ca path prints can be used from anywhere. *)
let absolute path =
  Bactrian_runtime.Proc_util.absolute
    (if String.starts_with ~prefix:"./" path then
       String.sub path 2 (String.length path - 2)
     else path)

let make ~dir ~signer ~cert =
  {
    signer;
    cert;
    path = absolute (Filename.concat dir certificate_file);
    leaves = Hosts.create 8;
    lock = Eio.Mutex.create ();
  }

(* The CA's directory and its parent, [.bactrian], as the store makes it. *)
let rec ensure_dir ?(parent = true) dir =
  match Unix.mkdir dir 0o700 with
  | () -> Ok ()
  | exception Unix.Unix_error (Unix.EEXIST, _, _) -> Ok ()
  | exception Unix.Unix_error (Unix.ENOENT, _, _) when parent ->
      let* () = ensure_dir ~parent:false (Filename.dirname dir) in
      ensure_dir ~parent:false dir
  | exception Unix.Unix_error (e, _, _) ->
      Error (Printf.sprintf "%s: %s" dir (Unix.error_message e))

let create ~dir ~now =
  let* () = ensure_dir dir in
  let* () = private_to_us "the CA directory" dir in
  let* signer, cert = mint now in
  let key_path = Filename.concat dir key_file
  and cert_path = Filename.concat dir certificate_file in
  (* Both are written before either is renamed, so that a failure leaves the
     old pair, and the two renames are as close together as they can be. *)
  let* key_temp =
    stage ~perm:0o600 key_path (X509.Private_key.encode_pem signer)
  in
  let* cert_temp =
    stage ~perm:0o644 cert_path (X509.Certificate.encode_pem cert)
  in
  let* () = install ~temp:key_temp key_path in
  let* () = install ~temp:cert_temp cert_path in
  Ok (make ~dir ~signer ~cert)

let rotate = create

let load ~dir ~now =
  let key_path = Filename.concat dir key_file in
  let* () = private_to_us "the CA key" key_path in
  let* key_text = read key_path in
  let* signer = msg (X509.Private_key.decode_pem key_text) in
  let* cert_text = read (Filename.concat dir certificate_file) in
  let* cert = msg (X509.Certificate.decode_pem cert_text) in
  let public = X509.Public_key.encode_der in
  if
    not
      (String.equal
         (public (X509.Private_key.public signer))
         (public (X509.Certificate.public_key cert)))
  then
    Error
      (Printf.sprintf
         "%s and %s in %s do not belong together: run bactrian ca rotate"
         key_file certificate_file dir)
  else if Ptime.is_later now ~than:(snd (X509.Certificate.validity cert)) then
    Error
      (Printf.sprintf "the project CA in %s has expired: run bactrian ca rotate"
         dir)
  else Ok (make ~dir ~signer ~cert)

let load_or_create ~dir ~now =
  let exists name = Sys.file_exists (Filename.concat dir name) in
  let* () =
    if Sys.file_exists dir then private_to_us "the CA directory" dir else Ok ()
  in
  match (exists key_file, exists certificate_file) with
  | true, true -> Result.map (fun t -> (t, Loaded)) (load ~dir ~now)
  | false, false -> Result.map (fun t -> (t, Created)) (create ~dir ~now)
  | true, false | false, true ->
      Error
        (Printf.sprintf "%s holds only one of %s and %s: run bactrian ca rotate"
           dir key_file certificate_file)

let certificate_pem t = X509.Certificate.encode_pem t.cert
let certificate_path t = t.path
let not_after t = snd (X509.Certificate.validity t.cert)

let fingerprint t =
  Bactrian_runtime.Proc_util.hex (X509.Certificate.fingerprint `SHA256 t.cert)

(* Leaves *)

let subject_alt_name host =
  match Ipaddr.of_string host with
  | Ok ip -> Ok X509.General_name.(singleton IP [ Ipaddr.to_octets ip ])
  | Error (`Msg _) -> (
      match Result.bind (Domain_name.of_string host) Domain_name.host with
      | Ok _ -> Ok X509.General_name.(singleton DNS [ host ])
      | Error (`Msg reason) ->
          Error (Printf.sprintf "%S is not a host name: %s" host reason))

let mint_leaf t ~valid_from ~until ~host =
  let* san = subject_alt_name host in
  let signer = X509.Private_key.generate `P256 in
  let ca_id = X509.Public_key.id (X509.Private_key.public t.signer) in
  let extensions =
    X509.Extension.(
      add Subject_alt_name (false, san)
        (add Basic_constraints
           (true, (false, None))
           (add Key_usage
              (true, [ `Digital_signature ])
              (add Ext_key_usage (false, [ `Server_auth ])
                 (add Authority_key_id
                    (false, (Some ca_id, X509.General_name.empty, None))
                    (singleton Subject_key_id
                       ( false,
                         X509.Public_key.id (X509.Private_key.public signer) )))))))
  in
  let valid_until =
    if Ptime.is_later until ~than:(not_after t) then not_after t else until
  in
  let* cert =
    sign ~issuer:t.signer
      ~issuer_dn:(X509.Certificate.subject t.cert)
      ~subject_key:signer ~subject:(dn host) ~extensions ~valid_from
      ~valid_until
  in
  Ok (([ cert; t.cert ], signer), valid_until)

(* Valid at both times: the agent may read the real clock or, under
   libfaketime, the run's simulated one. *)
let leaf t ~now ~sim_now ~host =
  let host = String.lowercase_ascii host in
  let earliest, latest =
    if Ptime.is_earlier sim_now ~than:now then (sim_now, now) else (now, sim_now)
  in
  Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
      match Hosts.find_opt t.leaves host with
      | Some (chain, from, until)
        when Ptime.is_earlier from ~than:(shift earliest (-1))
             && Ptime.is_later until ~than:(shift latest 1) ->
          Ok chain
      | Some _ | None ->
          let valid_from = shift earliest (-2) in
          let* chain, until =
            mint_leaf t ~valid_from ~until:(shift latest 7) ~host
          in
          Hosts.replace t.leaves host (chain, valid_from, until);
          Ok chain)
