open Bactrian_core
open Bactrian_runtime

type session = {
  run_id : string;
  namespace : string option;
  env : (string * string) list;
  start_body : string;
  mutable model_status : int option;
}

type t = { port : int; sessions : session list ref }

let json_response ?(status = `OK) body =
  Cohttp_eio.Server.respond_string ~status ~body ()

let read_body body =
  match Eio.Buf_read.parse ~max_size:max_int Eio.Buf_read.take_all body with
  | Ok s -> s
  | Error (`Msg _) -> ""

let header request name = Http.Header.get (Http.Request.headers request) name

let model_call ~sw ~net ~clock ~model_request (s : session) =
  let var name = Option.value (List.assoc_opt name s.env) ~default:"" in
  let status = ref 0 in
  let (_ : (unit, Upstream.error) result) =
    Upstream.call ~sw ~net ~clock ~timeout:10. Upstream.plain
      {
        meth = "POST";
        url = Uri.of_string (var "OPENAI_BASE_URL" ^ "/chat/completions");
        headers =
          [
            ("Content-Type", "application/json");
            ("Authorization", "Bearer " ^ var "OPENAI_API_KEY");
          ];
        body = model_request;
      }
      ~on_head:(fun head -> status := head.status)
      ~on_chunk:(fun ~dt_ms:_ _ -> ())
  in
  s.model_status <- Some !status

let start ~sw ~net ~clock ~namespace_header ~start_delay ~model_request =
  let sessions = ref [] in
  let callback _conn request body =
    let body = read_body body in
    let run_id = Option.value (header request "x-bactrian-run") ~default:"" in
    match Agent_protocol.decode_to_agent body with
    | Error _ | Ok (Start { env = None; _ }) ->
        json_response ~status:`Bad_request "[]"
    | Ok (Start { env = Some env; _ }) ->
        sessions :=
          !sessions
          @ [
              {
                run_id;
                namespace = header request namespace_header;
                env;
                start_body = body;
                model_status = None;
              };
            ];
        Eio.Time.sleep clock start_delay;
        json_response "[]"
    | Ok (Turn { turn; _ }) -> (
        match
          List.find_opt (fun s -> String.equal s.run_id run_id) !sessions
        with
        | None -> json_response ~status:`Not_found "[]"
        | Some s ->
            model_call ~sw ~net ~clock ~model_request s;
            let content =
              Printf.sprintf "model status %d"
                (Option.value s.model_status ~default:0)
            in
            json_response
              (Agent_protocol.encode_array [ Reply { turn; content } ]))
    | Ok End -> json_response "[]"
  in
  let sock =
    Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr sock with
    | `Tcp (_, port) -> port
    | `Unix _ -> 0
  in
  let server = Cohttp_eio.Server.make ~callback () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Cohttp_eio.Server.run sock server ~on_error:(fun _ -> ()));
  { port; sessions }

let url t = Printf.sprintf "http://127.0.0.1:%d/agent" t.port
let sessions t = !(t.sessions)
