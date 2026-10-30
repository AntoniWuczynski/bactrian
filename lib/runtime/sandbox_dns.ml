module Dns = Bactrian_core.Dns_message

let addresses =
  List.map Eio.Net.Ipaddr.of_raw [ "\127\000\000\001"; "\127\000\000\053" ]

let port = 53
let transport_name = function `Udp -> "udp" | `Tcp -> "tcp"

let line ~transport query =
  Yojson.Safe.to_string
    (`Assoc
       ([
          ("kind", `String "dns");
          ("transport", `String (transport_name transport));
        ]
       @
       match query with
       | Ok (q : Dns.question) ->
           [
             ("name", `String q.name); ("type", `String (Dns.type_name q.qtype));
           ]
       | Error e -> [ ("error", `String (Dns.describe_error e)) ]))

let nameservers text =
  String.split_on_char '\n' text
  |> List.filter_map (fun line ->
      match
        String.split_on_char ' '
          (String.map (function '\t' -> ' ' | c -> c) line)
        |> List.filter (fun w -> not (String.equal w ""))
      with
      | "nameserver" :: address :: _ -> Some address
      | _ -> None)

(* Dotted decimal only: the C library reads a part with a leading zero as
   octal. *)
let loopback address =
  let part p =
    match int_of_string_opt p with
    | Some n
      when String.length p <= 3
           && String.for_all (fun c -> c >= '0' && c <= '9') p
           && (String.length p = 1 || not (Char.equal p.[0] '0'))
           && n <= 255 ->
        Some (Char.chr n)
    | Some _ | None -> None
  in
  match List.map part (String.split_on_char '.' address) with
  | [ Some ('\127' as a); Some b; Some c; Some d ] ->
      Some (Eio.Net.Ipaddr.of_raw (String.of_seq (List.to_seq [ a; b; c; d ])))
  | _ -> None

(* Errs towards a gap: [0.0.0.0], which the kernel sends to 127.0.0.1, a short
   form such as [127.1], and a server glibc never asks (after the third) each
   count as unanswered, which only makes [complete] false. *)
let unanswered_nameservers text =
  List.filter (fun a -> Option.is_none (loopback a)) (nameservers text)

let listened text =
  let same (a : Eio.Net.Ipaddr.v4v6) (b : Eio.Net.Ipaddr.v4v6) =
    String.equal (a :> string) (b :> string)
  in
  List.fold_left
    (fun acc ip -> if List.exists (same ip) acc then acc else acc @ [ ip ])
    addresses
    (List.filter_map loopback (nameservers text))

(* The file as the sandbox shows it. *)
let resolv_conf path =
  match Unix.realpath path with
  | exception Unix.Unix_error _ -> ""
  | real when String.starts_with ~prefix:"/run/" real -> ""
  | real -> (
      match In_channel.with_open_bin real In_channel.input_all with
      | exception Sys_error _ -> ""
      | text -> text)

let unrecorded_resolvers ?(path = "/etc/resolv.conf") () =
  unanswered_nameservers (resolv_conf path)

let listen_addresses ?(path = "/etc/resolv.conf") () =
  listened (resolv_conf path)

type error = { address : string; reason : string }

let describe_error { address; reason } =
  Printf.sprintf "cannot listen for DNS on %s: %s" address reason

(* The largest DNS message, whose length a TCP prefix can give. *)
let max_message = 65535

let answer ~record ~transport message =
  record (line ~transport (Dns.parse message));
  Dns.refused message

(* An agent that is gone when its answer arrives, or a source the answer cannot
   be sent to, is no reason to stop. An error that every receive gives is, as
   the loop would otherwise spin. *)
let udp_loop ~record sock =
  let buf = Cstruct.create max_message in
  let rec loop failures =
    match Eio.Net.recv sock buf with
    | from, n ->
        (match
           answer ~record ~transport:`Udp (Cstruct.to_string ~len:n buf)
         with
        | Some reply -> (
            try Eio.Net.send sock ~dst:from [ Cstruct.of_string reply ]
            with Eio.Io _ -> ())
        | None -> ());
        loop 0
    | exception (Eio.Io _ as e) ->
        if failures < 100 then loop (failures + 1)
        else begin
          prerr_endline
            ("bactrian sandbox-init: DNS over UDP stopped: "
           ^ Printexc.to_string e);
          `Stop_daemon
        end
  in
  loop 0

let length_prefixed text =
  let n = String.length text in
  String.make 1 (Char.chr (n lsr 8))
  ^ String.make 1 (Char.chr (n land 0xff))
  ^ text

let tcp_connection ~record flow =
  let r = Eio.Buf_read.of_flow ~max_size:(max_message + 2) flow in
  let rec loop () =
    match Eio.Buf_read.take 2 r with
    | exception (End_of_file | Eio.Io _) -> ()
    | prefix -> (
        let n = (Char.code prefix.[0] lsl 8) lor Char.code prefix.[1] in
        match Eio.Buf_read.take n r with
        | exception (End_of_file | Eio.Io _) -> ()
        | message -> (
            match answer ~record ~transport:`Tcp message with
            | None -> loop ()
            | Some reply -> (
                match Eio.Flow.copy_string (length_prefixed reply) flow with
                | () -> loop ()
                | exception Eio.Io _ -> ())))
  in
  loop ()

let bound_port sock ~requested =
  match Eio_unix.Resource.fd_opt sock with
  | None -> requested
  | Some fd -> (
      match Eio_unix.Fd.use_exn "dns socket" fd Unix.getsockname with
      | ADDR_INET (_, p) -> p
      | ADDR_UNIX _ -> requested)

let bind ~sw ~net ~on_bound (ip, port) =
  let address = Format.asprintf "%a:%d" Eio.Net.Ipaddr.pp ip port in
  match
    let udp =
      Eio.Net.datagram_socket ~reuse_addr:true ~sw net (`Udp (ip, port))
    in
    let tcp =
      Eio.Net.listen ~sw ~reuse_addr:true ~backlog:32 net (`Tcp (ip, port))
    in
    let tcp_port =
      match Eio.Net.listening_addr tcp with `Tcp (_, p) -> p | `Unix _ -> port
    in
    (udp, bound_port udp ~requested:port, tcp, tcp_port)
  with
  | exception ((Eio.Io _ | Unix.Unix_error _) as e) ->
      Error { address; reason = Printexc.to_string e }
  | udp, udp_port, tcp, tcp_port ->
      on_bound `Udp udp_port;
      on_bound `Tcp tcp_port;
      Ok (udp, tcp)

let serve ?(on_bound = fun (_ : [ `Udp | `Tcp ]) (_ : int) -> ()) ~sw ~net
    ~record addresses =
  let rec bind_all acc = function
    | [] -> Ok (List.rev acc)
    | a :: rest -> (
        match bind ~sw ~net ~on_bound a with
        | Ok pair -> bind_all (pair :: acc) rest
        | Error _ as e -> e)
  in
  Result.map
    (List.iter (fun (udp, tcp) ->
         Eio.Fiber.fork_daemon ~sw (fun () -> udp_loop ~record udp);
         Eio.Fiber.fork_daemon ~sw (fun () ->
             Eio.Net.run_server tcp
               ~on_error:(fun e ->
                 prerr_endline
                   ("bactrian sandbox-init: DNS connection failed: "
                  ^ Printexc.to_string e))
               (fun flow _ -> tcp_connection ~record flow))))
    (bind_all [] addresses)
