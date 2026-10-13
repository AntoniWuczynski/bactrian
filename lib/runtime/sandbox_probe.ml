type outcome = (unit, string) result

type report = {
  public : outcome;
  listener : outcome;
  outside_write : outcome;
  inside_write : outcome;
  netns : string option;
}

let greeting = "bactrian-probe\n"
let timeout = 5.

(* Unix.error has dozens of constructors and the probe names only those its
   checks can meet, so the match ends in a catch-all. *)
let[@warning "-4"] error_name : Unix.error -> string = function
  | EPERM -> "EPERM"
  | EACCES -> "EACCES"
  | EROFS -> "EROFS"
  | ENOENT -> "ENOENT"
  | EEXIST -> "EEXIST"
  | ENETUNREACH -> "ENETUNREACH"
  | EHOSTUNREACH -> "EHOSTUNREACH"
  | ECONNREFUSED -> "ECONNREFUSED"
  | ETIMEDOUT -> "ETIMEDOUT"
  | EADDRNOTAVAIL -> "EADDRNOTAVAIL"
  | other -> Unix.error_message other

let attempt f =
  match f () with
  | () -> Ok ()
  | exception Unix.Unix_error (e, _, _) -> Error (error_name e)

let wait_ready fd ~write =
  let r, w = if write then ([], [ fd ]) else ([ fd ], []) in
  match Unix.select r w [] timeout with [], [], [] -> false | _ -> true

(* Connects without sending anything, then hands the socket to [use]. *)
let connect addr use =
  let fd = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
  Unix.set_nonblock fd;
  let connected =
    match Unix.connect fd addr with
    | () -> Ok ()
    | exception Unix.Unix_error ((EINPROGRESS | EWOULDBLOCK | EAGAIN), _, _)
      -> (
        if not (wait_ready fd ~write:true) then Error "timeout"
        else
          match Unix.getsockopt_error fd with
          | None -> Ok ()
          | Some e -> Error (error_name e))
    | exception Unix.Unix_error (e, _, _) -> Error (error_name e)
  in
  Result.bind connected (fun () -> use fd)

let read_greeting fd =
  let buf = Bytes.create (String.length greeting) in
  let rec go off =
    if off = Bytes.length buf then
      if String.equal (Bytes.to_string buf) greeting then Ok ()
      else Error "unexpected reply"
    else if not (wait_ready fd ~write:false) then Error "timeout"
    else
      match Unix.read fd buf off (Bytes.length buf - off) with
      | 0 -> Error "closed before the greeting"
      | n -> go (off + n)
      | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> go off
      | exception Unix.Unix_error (e, _, _) -> Error (error_name e)
  in
  go 0

let create path =
  attempt (fun () ->
      let fd =
        Unix.openfile path [ O_WRONLY; O_CREAT; O_EXCL; O_CLOEXEC ] 0o600
      in
      Unix.close fd)

let run ~port ~outside ~inside =
  let public =
    connect
      (ADDR_INET (Unix.inet_addr_of_string "1.1.1.1", 443))
      (fun _ -> Ok ())
  in
  let listener =
    connect (ADDR_INET (Unix.inet_addr_loopback, port)) read_greeting
  in
  let netns =
    match Unix.readlink "/proc/self/ns/net" with
    | ns -> Some ns
    | exception Unix.Unix_error _ -> None
  in
  {
    public;
    listener;
    outside_write = create outside;
    inside_write = create inside;
    netns;
  }

(* The wire form: each outcome is "ok" or the error's name. *)

type wire = {
  public : string;
  listener : string;
  outside_write : string;
  inside_write : string;
  netns : string option; [@default None]
}
[@@deriving yojson]

let ok_word = "ok"
let to_word = function Ok () -> ok_word | Error e -> e
let of_word w = if String.equal w ok_word then Ok () else Error w

let to_line (r : report) =
  Yojson.Safe.to_string
    (wire_to_yojson
       {
         public = to_word r.public;
         listener = to_word r.listener;
         outside_write = to_word r.outside_write;
         inside_write = to_word r.inside_write;
         netns = r.netns;
       })

let of_line line =
  match Yojson.Safe.from_string line with
  | exception Yojson.Json_error m -> Error m
  | json ->
      Result.map
        (fun (w : wire) : report ->
          {
            public = of_word w.public;
            listener = of_word w.listener;
            outside_write = of_word w.outside_write;
            inside_write = of_word w.inside_write;
            netns = w.netns;
          })
        (wire_of_yojson json)

let judge (platform : Bactrian_core.Sandbox_policy.platform) ~host_netns
    (r : report) =
  let public =
    match (r.public, platform) with
    | Ok (), (Macos | Linux) ->
        [ "an outbound connection to 1.1.1.1:443 succeeded" ]
    | Error ("EPERM" | "EACCES"), Macos -> []
    | Error e, Macos ->
        [
          Printf.sprintf
            "an outbound connection to 1.1.1.1:443 failed with %s instead of \
             the sandbox's EPERM, so something else stopped it"
            e;
        ]
    | Error _, Linux -> (
        match (r.netns, host_netns) with
        | Some inside, Some outside when not (String.equal inside outside) -> []
        | Some _, Some _ | None, _ | _, None ->
            [ "the probe was not in a network namespace of its own" ])
  in
  let must_work what = function
    | Ok () -> []
    | Error e -> [ Printf.sprintf "%s failed (%s)" what e ]
  in
  let must_fail what = function
    | Ok () -> [ what ^ " succeeded" ]
    | Error _ -> []
  in
  match
    public
    @ must_work "a connection to bactrian's own listener" r.listener
    @ must_fail "a write outside the state directory" r.outside_write
    @ must_work "a write inside the state directory" r.inside_write
  with
  | [] -> Ok ()
  | wrong -> Error wrong
