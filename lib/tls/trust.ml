let ( let* ) = Result.bind
let extra_variable = "BACTRIAN_EXTRA_CA_CERTS"

(* The paths of ca-certs 1.0.3 (lib/ca_certs.ml, from Go's root_linux.go), with
   Alpine's and OpenBSD's /etc/ssl/cert.pem last. *)
let linux_bundles =
  [
    "/etc/ssl/certs/ca-certificates.crt";
    "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem";
    "/etc/ssl/ca-bundle.pem";
    "/etc/ssl/cert.pem";
  ]

let macos_roots_keychain =
  "/System/Library/Keychains/SystemRootCertificates.keychain"

let macos_admin_keychain = "/Library/Keychains/System.keychain"
let security = "/usr/bin/security"

(* Every macOS has this file, and Linux does not. *)
let macos () =
  Sys.file_exists "/System/Library/CoreServices/SystemVersion.plist"

let read_file path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> Ok text
  | exception Sys_error reason -> Error reason

(* The certificates, and how many PEM blocks did not decode. *)
let decode text =
  let certs, skipped =
    X509.Certificate.fold_decode_pem_multiple
      (fun (certs, skipped) -> function
        | Ok c -> (c :: certs, skipped) | Error (`Msg _) -> (certs, skipped + 1))
      ([], 0) text
  in
  (List.rev certs, skipped)

let certs text = fst (decode text)

let keychain ~process_mgr path =
  match
    Eio.Process.parse_out process_mgr Eio.Buf_read.take_all ~env:[||]
      ~executable:security
      [ security; "find-certificate"; "-a"; "-p"; path ]
  with
  | text -> Ok text
  | exception (Eio.Io _ as e) -> Error (Printexc.to_string e)

let none_found paths =
  Error
    (Printf.sprintf "no trusted root certificates were found in %s"
       (String.concat ", " paths))

(* The admin keychain may hold no certificate, which [security] reports as a
   failure, so only the system roots keychain must be read. *)
let macos_roots ~process_mgr =
  match keychain ~process_mgr macos_roots_keychain with
  | Error reason ->
      Error
        (Printf.sprintf "%s cannot be read with %s: %s" macos_roots_keychain
           security reason)
  | Ok text -> (
      match certs text with
      | [] -> none_found [ macos_roots_keychain ]
      | _ :: _ as roots ->
          Ok
            (roots
            @ Result.fold ~ok:certs
                ~error:(fun (_ : string) -> [])
                (keychain ~process_mgr macos_admin_keychain)))

let linux_roots () =
  match
    List.find_map
      (fun path ->
        match Result.map certs (read_file path) with
        | Ok (_ :: _ as roots) -> Some roots
        | Ok [] | Error _ -> None)
      linux_bundles
  with
  | Some roots -> Ok roots
  | None -> none_found linux_bundles

let system ~process_mgr =
  if macos () then macos_roots ~process_mgr else linux_roots ()

(* The extra roots are trusted for every host, so a file another user can
   change is refused, as ssh refuses such a key file. The checks are on the
   open descriptor, so the file read is the file checked. *)
let read_extra path =
  match Unix.openfile path [ O_RDONLY; O_CLOEXEC ] 0 with
  | exception Unix.Unix_error (e, _, _) ->
      Error
        (Printf.sprintf "%s cannot be read: %s: %s" extra_variable path
           (Unix.error_message e))
  | fd -> (
      let ic = Unix.in_channel_of_descr fd in
      Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () ->
      let { Unix.st_uid; st_perm; _ } = Unix.fstat fd in
      if not (Int.equal st_uid (Unix.geteuid ()) || Int.equal st_uid 0) then
        Error
          (Printf.sprintf
             "%s names %s, which belongs to another user, so it is not trusted"
             extra_variable path)
      else if st_perm land 0o022 <> 0 then
        Error
          (Printf.sprintf
             "%s names %s, which other users can write to (mode %o), so it is \
              not trusted"
             extra_variable path st_perm)
      else
        match In_channel.input_all ic with
        | text -> Ok text
        | exception Sys_error reason ->
            Error
              (Printf.sprintf "%s cannot be read: %s: %s" extra_variable path
                 reason))

type extra = { path : string; certs : X509.Certificate.t list; skipped : int }

let extra () =
  match Sys.getenv_opt extra_variable with
  | None | Some "" -> Ok None
  | Some path -> (
      let* text = read_extra path in
      match decode text with
      | [], _ ->
          Error
            (Printf.sprintf "%s names a file with no PEM certificate"
               extra_variable)
      | certs, skipped -> Ok (Some { path; certs; skipped }))

type t = { system : X509.Certificate.t list; extra : extra option }

let load ~process_mgr =
  let* system = system ~process_mgr in
  let* extra = extra () in
  Ok { system; extra }
