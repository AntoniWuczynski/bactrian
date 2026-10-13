type ending = Complete | Drop

type reply = {
  status : int;
  headers : (string * string) list;
  chunks : string list;
  ending : ending;
}

type source = Fixtures of string | Script of reply list

type error =
  | Unreadable of { path : string; reason : string }
  | Invalid_fixture of { case : string; reason : string }

let pp_error ppf = function
  | Unreadable { path; reason } ->
      Format.fprintf ppf "cannot read %s: %s" path reason
  | Invalid_fixture { case; reason } ->
      Format.fprintf ppf "invalid fixture %s: %s" case reason

type request = {
  meth : string;
  path : string;
  query : string;
  headers : (string * string) list;
  body : string;
}

type fixture = {
  meth : string;
  path : string;
  query : string;
  body : Bactrian_core.Canon.t;
  reply : reply;
}

module Canon = Bactrian_core.Canon

(* Raised from the response writer to make cohttp drop the connection without
   the terminating chunk. It ends one connection only: the server's [on_error]
   swallows it. *)
exception Dropped

(* Fixture loading. [Sys_error] is the documented failure of the file
   functions and is turned into [Unreadable] right here. *)

let ( let* ) = Result.bind

let read_file path =
  match In_channel.with_open_bin path In_channel.input_all with
  | content -> Ok content
  | exception Sys_error reason -> Error (Unreadable { path; reason })

let list_dir path =
  match Sys.readdir path with
  | names ->
      Array.sort String.compare names;
      Ok (Array.to_list names)
  | exception Sys_error reason -> Error (Unreadable { path; reason })

let json ~case path =
  let* text = read_file path in
  Result.map_error
    (fun e ->
      Invalid_fixture
        { case; reason = Format.asprintf "%s: %a" path Canon.pp_error e })
    (Canon.of_string text)

let field ~case name value =
  Option.to_result
    ~none:(Invalid_fixture { case; reason = "meta.json lacks " ^ name })
    value

(* Cut [raw] at the recorded chunk boundaries, which must tile it exactly. *)
let split_chunks ~case raw boundaries =
  let invalid reason = Error (Invalid_fixture { case; reason }) in
  let rec go pos acc = function
    | [] ->
        if pos = String.length raw then Ok (List.rev acc)
        else invalid "chunks.json does not cover response.raw"
    | b :: rest -> (
        match (Canon.int_member "offset" b, Canon.int_member "length" b) with
        | Some offset, Some length
          when offset = pos && length >= 0 && pos + length <= String.length raw
          ->
            go (pos + length) (String.sub raw pos length :: acc) rest
        | Some _, Some _ -> invalid "chunks.json boundaries are not contiguous"
        | None, _ | _, None -> invalid "chunk lacks offset or length")
  in
  go 0 [] boundaries

let load_case dir =
  let case =
    Filename.basename (Filename.dirname dir) ^ "/" ^ Filename.basename dir
  in
  let file name = Filename.concat dir name in
  let* meta = json ~case (file "meta.json") in
  let* body = json ~case (file "request.json") in
  let* boundaries = json ~case (file "chunks.json") in
  let* raw = read_file (file "response.raw") in
  let* meth = field ~case "method" (Canon.str_member "method" meta) in
  let* path = field ~case "path" (Canon.str_member "path" meta) in
  let* status = field ~case "status" (Canon.int_member "status" meta) in
  let query = Option.value ~default:"" (Canon.str_member "query" meta) in
  let content_type =
    Option.bind
      (Canon.member "response_headers" meta)
      (Canon.str_member "content-type")
  in
  let* boundaries =
    field ~case "chunks.json list" (Canon.to_list_opt boundaries)
  in
  let* chunks = split_chunks ~case raw boundaries in
  let headers =
    Option.fold ~none:[] ~some:(fun ct -> [ ("content-type", ct) ]) content_type
  in
  Ok
    {
      meth;
      path;
      query;
      body;
      reply = { status; headers; chunks; ending = Complete };
    }

let load_fixtures root =
  let* protocols = list_dir root in
  List.fold_left
    (fun acc protocol ->
      let* fixtures = acc in
      let protocol_dir = Filename.concat root protocol in
      if not (Sys.is_directory protocol_dir) then Ok fixtures
      else
        let* cases = list_dir protocol_dir in
        List.fold_left
          (fun acc case ->
            let* fixtures = acc in
            let* fixture = load_case (Filename.concat protocol_dir case) in
            Ok (fixture :: fixtures))
          (Ok fixtures) cases)
    (Ok []) protocols
  |> Result.map List.rev

(* Serving *)

type t = { port : int; log : request list ref  (** Newest first. *) }

let message ~status text =
  let body = Printf.sprintf {|{"error":{"message":%S}}|} text in
  {
    status;
    headers = [ ("content-type", "application/json") ];
    chunks = [ body ];
    ending = Complete;
  }

(* Written by hand, not through a body source, so that each recorded chunk is
   exactly one HTTP chunk that is flushed on its own. *)
let respond (reply : reply) =
  let headers =
    Http.Header.add_list
      (Http.Header.of_list reply.headers)
      [ ("transfer-encoding", "chunked") ]
  in
  let response =
    Http.Response.make ~status:(Http.Status.of_int reply.status) ~headers ()
  in
  ( response,
    fun _ic oc ->
      List.iter
        (fun chunk ->
          if not (String.equal chunk "") then begin
            Eio.Buf_write.printf oc "%x\r\n" (String.length chunk);
            Eio.Buf_write.string oc chunk;
            Eio.Buf_write.string oc "\r\n";
            Eio.Buf_write.flush oc
          end)
        reply.chunks;
      match reply.ending with
      | Complete ->
          Eio.Buf_write.string oc "0\r\n\r\n";
          Eio.Buf_write.flush oc
      | Drop -> raise Dropped )

let find_fixture fixtures (r : request) =
  match Canon.of_string r.body with
  | Error _ -> Error (message ~status:400 "request body is not JSON")
  | Ok body -> (
      match
        List.find_opt
          (fun (f : fixture) ->
            String.equal f.meth r.meth && String.equal f.path r.path
            && String.equal f.query r.query
            && Canon.equal f.body body)
          fixtures
      with
      | Some f -> Ok f.reply
      | None -> Error (message ~status:404 "no fixture matches this request"))

let choose source ~count r =
  match source with
  | `Fixtures fixtures -> (
      match find_fixture fixtures r with Ok reply | Error reply -> reply)
  | `Script replies -> (
      match List.nth_opt replies count with
      | Some reply -> reply
      | None -> message ~status:500 "script exhausted")

let start ~sw ~net source =
  let* source =
    match source with
    | Fixtures dir -> Result.map (fun f -> `Fixtures f) (load_fixtures dir)
    | Script replies -> Ok (`Script replies)
  in
  let log = ref [] in
  let callback _conn http_request body =
    let body =
      match Eio.Buf_read.parse ~max_size:max_int Eio.Buf_read.take_all body with
      | Ok s -> s
      | Error (`Msg _) -> ""
    in
    let uri = Uri.of_string (Http.Request.resource http_request) in
    let r =
      {
        meth = Http.Method.to_string (Http.Request.meth http_request);
        path = Uri.path uri;
        query = Option.value ~default:"" (Uri.verbatim_query uri);
        headers =
          List.map
            (fun (k, v) -> (String.lowercase_ascii k, v))
            (Http.Header.to_list (Http.Request.headers http_request));
        body;
      }
    in
    let count = List.length !log in
    log := r :: !log;
    respond (choose source ~count r)
  in
  let sock =
    Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  match Eio.Net.listening_addr sock with
  | `Unix path -> Error (Unreadable { path; reason = "listener is not TCP" })
  | `Tcp (_, port) ->
      let server = Cohttp_eio.Server.make_expert ~callback () in
      Eio.Fiber.fork_daemon ~sw (fun () ->
          (* [Dropped] and client disconnects end one connection only. *)
          Cohttp_eio.Server.run sock server ~on_error:(fun _ -> ()));
      Ok { port; log }

let port t = t.port
let base_url t = Printf.sprintf "http://127.0.0.1:%d" t.port
let requests t = List.rev !(t.log)
