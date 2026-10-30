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

let macos_keychains =
  [
    "/System/Library/Keychains/SystemRootCertificates.keychain";
    "/Library/Keychains/System.keychain";
  ]

let security = "/usr/bin/security"

(* Every macOS has this file, and Linux does not. *)
let macos () =
  Sys.file_exists "/System/Library/CoreServices/SystemVersion.plist"

let read_file path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> Ok text
  | exception Sys_error reason -> Error reason

let decode text =
  List.rev
    (X509.Certificate.fold_decode_pem_multiple
       (fun acc -> function Ok c -> c :: acc | Error (`Msg _) -> acc)
       [] text)

let keychain ~process_mgr path =
  match
    Eio.Process.parse_out process_mgr Eio.Buf_read.take_all ~env:[||]
      ~executable:security
      [ security; "find-certificate"; "-a"; "-p"; path ]
  with
  | text -> Ok text
  | exception (Eio.Io _ as e) -> Error (Printexc.to_string e)

let system ~process_mgr =
  let macos = macos () in
  let roots =
    if macos then
      List.concat_map
        (fun path ->
          Result.fold ~ok:decode
            ~error:(fun (_ : string) -> [])
            (keychain ~process_mgr path))
        macos_keychains
    else
      Option.value ~default:[]
        (List.find_map
           (fun path ->
             match Result.map decode (read_file path) with
             | Ok (_ :: _ as roots) -> Some roots
             | Ok [] | Error _ -> None)
           linux_bundles)
  in
  match roots with
  | [] ->
      Error
        (Printf.sprintf "no trusted root certificates were found in %s"
           (String.concat ", "
              (if macos then macos_keychains else linux_bundles)))
  | _ :: _ -> Ok roots

let extra () =
  match Sys.getenv_opt extra_variable with
  | None | Some "" -> Ok []
  | Some path -> (
      match read_file path with
      | Error reason ->
          Error (Printf.sprintf "%s cannot be read: %s" extra_variable reason)
      | Ok text -> (
          match decode text with
          | [] ->
              Error
                (Printf.sprintf "%s names a file with no PEM certificate"
                   extra_variable)
          | certs -> Ok certs))

type t = { system : X509.Certificate.t list; extra : X509.Certificate.t list }

let load ~process_mgr =
  let* system = system ~process_mgr in
  let* extra = extra () in
  Ok { system; extra }
