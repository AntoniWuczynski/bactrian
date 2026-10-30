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

let answered = List.map (Format.asprintf "%a" Eio.Net.Ipaddr.pp) addresses

let unanswered_nameservers text =
  String.split_on_char '\n' text
  |> List.filter_map (fun line ->
      match
        String.split_on_char ' '
          (String.map (function '\t' -> ' ' | c -> c) line)
        |> List.filter (fun w -> not (String.equal w ""))
      with
      | "nameserver" :: address :: _
        when not (List.exists (String.equal address) answered) ->
          Some address
      | _ -> None)

let unrecorded_resolvers ?(path = "/etc/resolv.conf") () =
  match Unix.realpath path with
  | exception Unix.Unix_error _ -> []
  | real when String.starts_with ~prefix:"/run/" real -> []
  | real -> (
      match In_channel.with_open_bin real In_channel.input_all with
      | exception Sys_error _ -> []
      | text -> unanswered_nameservers text)

type error = { address : string; reason : string }

let describe_error { address; reason } =
  Printf.sprintf "cannot listen for DNS on %s: %s" address reason

(* The largest DNS message, whose length a TCP prefix can give. *)
let max_message = 65535

let answer ~record ~transport message =
  record (line ~transport (Dns.parse message));
  Dns.refused message

(* An agent that is gone when its answer arrives, or a source the answer cannot
   be sent to, is no reason to stop. *)
let udp_loop ~record sock =
  let buf = Cstruct.create max_message in
  let rec loop () =
    (match Eio.Net.recv sock buf with
    | from, n -> (
        match answer ~record ~transport:`Udp (Cstruct.to_string ~len:n buf) with
        | Some reply -> (
            try Eio.Net.send sock ~dst:from [ Cstruct.of_string reply ]
            with Eio.Io _ -> ())
        | None -> ())
    | exception Eio.Io _ -> ());
    loop ()
  in
  loop ()

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
