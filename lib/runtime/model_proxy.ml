open Bactrian_core

type provider = Openai | Anthropic | Gemini | Openrouter

let providers = [ Openai; Anthropic; Gemini; Openrouter ]

let default_upstream p =
  Uri.of_string
    (match p with
    | Openai -> "https://api.openai.com"
    | Anthropic -> "https://api.anthropic.com"
    | Gemini -> "https://generativelanguage.googleapis.com"
    | Openrouter -> "https://openrouter.ai")

let segment = function
  | Openai -> "openai"
  | Anthropic -> "anthropic"
  | Gemini -> "gemini"
  | Openrouter -> "openrouter"

let provider_of_segment s =
  List.find_opt (fun p -> String.equal (segment p) s) providers

let provider_equal a b = String.equal (segment a) (segment b)

(* The part of the path that SDK base URLs include. *)
let sdk_base = function
  | Openai -> "/v1"
  | Openrouter -> "/api/v1"
  | Anthropic | Gemini -> ""

let key_header = function
  | Openai | Openrouter -> "authorization"
  | Anthropic -> "x-api-key"
  | Gemini -> "x-goog-api-key"

let key_value p key =
  match p with
  | Openai | Openrouter -> "Bearer " ^ key
  | Anthropic | Gemini -> key

type timing = Fast | Recorded

type mode =
  | Record
  | Replay of {
      recording : Cassette.t;
      matcher : Matcher.mode;
      allow_reorder : bool;
      timing : timing;
    }
  | Passthrough

type run = {
  run_id : string;
  token : string;
  mode : mode;
  store : Store.t;
  cassette : Store.writer option;
  ledger : Ledger.t;
  keys : (provider * string) list;
  upstream : provider -> Uri.t;
  connector : Upstream.connector;
  normalise : Re.re list;
  redact : Redact.pattern list;
  pricing : Pricing.table;
  run_started : float;
  mcp : Mcp_proxy.config list;
}

type findings = {
  divergences : Matcher.divergence list;
  exchanges : int;
  transport_errors : int;
  usage_gaps : int;
  refused_keys : int;
  masks_fired : int;
  cap_refusals : Ledger.refusal list;
  errors : string list;
}

module Counts = Map.Make (String)
module Runs = Hashtbl.Make (String)

type replaying = { mutable matcher : Matcher.state; timing : timing }
type serving = Live of { record : bool } | Replaying of replaying

(* Everything mutable about a run. Guarded by [lock]. *)
type state = {
  run : run;
  serving : serving;
  lock : Eio.Mutex.t;
  mutable redact : Redact.state;
  mutable seen : int Counts.t;  (** Requests so far per key. *)
  mutable next_seq : int;
  mutable divergences : Matcher.divergence list;  (** Newest first. *)
  mutable exchanges : int;
  mutable transport_errors : int;
  mutable usage_gaps : int;
  mutable refused_keys : int;
  mutable masks_fired : int;
  mutable cap_refusals : Ledger.refusal list;  (** Newest first. *)
  mutable errors : string list;  (** Newest first. *)
  mutable ended : bool;
      (** The run is unregistered: its cassette is closed, so a call that
          finishes late settles the ledger but records nothing. *)
  mcp : Mcp_proxy.t Lazy.t;  (** Forced by the run's first MCP request. *)
}

type t = {
  port : int;
  runs : state Runs.t;
  mutable unattributed : int;
  now : unit -> float;
  sleep : float -> unit;
  call :
    sw:Eio.Switch.t ->
    Upstream.connector ->
    Upstream.request ->
    on_head:(Upstream.head -> unit) ->
    on_chunk:(dt_ms:int -> string -> unit) ->
    (unit, Upstream.error) result;
  mcp_env : Mcp_proxy.env;
  max_body : int;
}

(* Runs are keyed by a digest of their token, so lookups never compare the
   token itself and take time that does not depend on how much of it matches. *)
let token_key token = Digest.BLAKE256.(to_hex (string token))
let locked st f = Eio.Mutex.use_rw ~protect:true st.lock f

(* [add_error] runs with the lock held, [note] takes it. *)
let add_error st message = st.errors <- message :: st.errors
let note st message = locked st (fun () -> add_error st message)

(* Ten minutes covers the slowest long generation. *)
let upstream_timeout = 600.

(* Responses. Bodies are always written chunk by chunk by hand so that each
   upstream piece reaches the agent as its own flushed HTTP chunk, and so that
   a dropped upstream can drop the agent's connection before the terminal
   chunk. *)

type item = Chunk of string | Finished | Dropped

exception Upstream_dropped

(* A failure of the agent's connection, already noted in its run's errors. *)
exception Attributed of exn

type response = Http.Response.t * (Eio.Buf_read.t -> Eio.Buf_write.t -> unit)

let respond ?on_exn ~status ~headers (next : unit -> item) : response =
  let headers =
    Http.Header.add (Http.Header.of_list headers) "transfer-encoding" "chunked"
  in
  ( Http.Response.make ~status:(Http.Status.of_int status) ~headers (),
    fun _ic oc ->
      Eio.Buf_write.flush oc;
      let attribute ex =
        match on_exn with
        | None -> ex
        | Some note_it ->
            note_it ex;
            Attributed ex
      in
      let rec loop () =
        match next () with
        | Chunk "" -> loop ()
        | Chunk s ->
            Eio.Buf_write.printf oc "%x\r\n" (String.length s);
            Eio.Buf_write.string oc s;
            Eio.Buf_write.string oc "\r\n";
            Eio.Buf_write.flush oc;
            loop ()
        | Finished ->
            Eio.Buf_write.string oc "0\r\n\r\n";
            Eio.Buf_write.flush oc
        | Dropped -> raise Upstream_dropped
      in
      try loop () with
      | (Eio.Cancel.Cancelled _ | Upstream_dropped) as ex -> raise ex
      | ex -> raise (attribute ex) )

let agent_failed st ex =
  note st ("connection to the agent failed: " ^ Printexc.to_string ex)

let of_list pieces =
  let rest = ref pieces in
  fun () ->
    match !rest with
    | [] -> Finished
    | x :: tl ->
        rest := tl;
        Chunk x

let obj = Canon.of_fields

let error_response ?(extra = []) ~status ~type_ message =
  let error =
    obj
      (("type", Canon.string type_)
      :: ("message", Canon.string message)
      :: extra)
  in
  respond ~status
    ~headers:[ ("content-type", "application/json") ]
    (of_list [ Canon.to_string (obj [ ("error", error) ]) ])

(* An error response for a request that belongs to a run: noted in its errors. *)
let refusal_response st ?extra ~status ~type_ message =
  note st message;
  error_response ?extra ~status ~type_ message

(* Request analysis *)

let next_occurrence st key =
  let n = Option.value (Counts.find_opt key st.seen) ~default:0 in
  st.seen <- Counts.add key (n + 1) st.seen;
  n

let t_ms t st = Float.to_int ((t.now () -. st.run.run_started) *. 1000.)

(* One model request on its way through the proxy. *)
type pending = {
  provider : provider;
  adapter : (module Protocol.S);
  meth : string;
  path : string;  (** Relative to the provider base. *)
  raw_query : string option;
  query : (string * string) list;  (** Normalised. *)
  headers : (string * string) list;  (** As the agent sent them. *)
  body : string;  (** As the agent sent it. *)
  scrubbed : string;
  parsed : Canon.t;  (** [scrubbed], parsed. *)
  model : string;  (** From the unmasked canonical request, for pricing. *)
  request : Matcher.request;
  occurrence : int;
  t_ms : int;
}

let pair_compare (n1, v1) (n2, v2) =
  match String.compare n1 n2 with 0 -> String.compare v1 v2 | n -> n

let normalised_query uri =
  Uri.query uri
  |> List.concat_map (fun (k, vs) ->
      match vs with
      | [] -> [ (k, "") ]
      | _ :: _ -> List.map (fun v -> (k, v)) vs)
  |> List.filter (fun (k, _) -> not (String.equal k "key"))
  |> List.sort pair_compare

let known_secrets run = run.token :: List.map snd run.keys

(* Provider key formats and the known literals are scrubbed in the raw text,
   where their boundary groups keep them exact. The user's patterns only see
   the strings of a JSON document (or of a server-sent event), because a
   pattern such as [Bearer .+] would otherwise swallow the closing quote and
   brace. *)
let scrub st text =
  let redact, text =
    Redact.scrub ~patterns:Redact.provider_key_patterns
      ~known:(known_secrets st.run) st.redact text
  in
  let redact, text =
    match st.run.redact with
    | [] -> (redact, text)
    | patterns -> Redact.scrub_structured ~patterns redact text
  in
  st.redact <- redact;
  text

let max_output_paths =
  [
    [ "max_tokens" ];
    [ "max_completion_tokens" ];
    [ "max_output_tokens" ];
    [ "generationConfig"; "maxOutputTokens" ];
    [ "generation_config"; "max_output_tokens" ];
  ]

let rec int_at v = function
  | [] -> Canon.to_int_opt v
  | k :: rest -> Option.bind (Canon.member k v) (fun c -> int_at c rest)

let max_output body = List.find_map (int_at body) max_output_paths

let date_of time =
  let tm = Unix.gmtime time in
  Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

let header_value name headers =
  List.find_map
    (fun (k, v) ->
      if String.equal (String.lowercase_ascii k) name then Some v else None)
    headers

(* Headers never forwarded: hop-by-hop, framing, cookies and every key header
   (the real key is added for the provider). *)
let dropped_headers =
  [
    "host";
    "content-length";
    "transfer-encoding";
    "connection";
    "keep-alive";
    "te";
    "upgrade";
    "accept-encoding";
    "proxy-authorization";
    "proxy-connection";
    "cookie";
    "authorization";
    "x-api-key";
    "x-goog-api-key";
  ]

let forward_headers headers p real_key =
  (key_header p, key_value p real_key)
  :: List.filter
       (fun (k, _) -> not (List.mem (String.lowercase_ascii k) dropped_headers))
       headers

let upstream_url run x =
  let base = Uri.to_string (run.upstream x.provider) in
  let base =
    if String.ends_with ~suffix:"/" base then
      String.sub base 0 (String.length base - 1)
    else base
  in
  Uri.of_string
    (base ^ x.path ^ match x.raw_query with Some q -> "?" ^ q | None -> "")

(* Response bodies for summaries *)

let is_sse headers =
  match header_value "content-type" headers with
  | Some ct -> String.starts_with ~prefix:"text/event-stream" ct
  | None -> false

let response_body x (head : Upstream.head) chunks : Protocol.body option =
  if is_sse head.headers then
    let st, rev =
      List.fold_left
        (fun (st, acc) piece ->
          let st, items = Sse.feed st piece in
          (st, List.rev_append items acc))
        (Sse.init, []) chunks
    in
    match Sse.finish st with
    | `Clean -> Some (Sse (List.rev rev))
    | `Truncated _ -> None
  else
    match Canon.of_string (String.concat "" chunks) with
    | Error _ -> None
    | Ok v -> (
        match (x.request.shape, Canon.to_list_opt v) with
        | Streamed, Some items -> Some (Json_stream items)
        | Streamed, None | Whole_body, (Some _ | None) -> Some (Json v))

(* The actual cost of a finished call, or [None] for a usage gap. Providers do
   not bill requests they reject, so an error status costs nothing. *)
let actual_cost x ~table ~model ~date (head : Upstream.head) chunks =
  if head.status >= 400 then Some 0.
  else
    let (module A : Protocol.S) = x.adapter in
    match Option.map A.summarise (response_body x head chunks) with
    | Some (Ok { usage = Some u; _ }) ->
        Reservation.cost ~table ~model ~date x.parsed u
    | Some (Ok { usage = None; _ }) | Some (Error _) | None -> None

(* Live calls *)

type finish = Upstream of (unit, Upstream.error) result | Agent_gone

let record_live st x ~head ~chunks finish =
  let outcome : Cassette.outcome =
    match (head, finish) with
    | None, Upstream (Error e) -> Transport_error (Upstream.describe_error e)
    | None, Upstream (Ok ()) -> Transport_error "no response head"
    | None, Agent_gone ->
        Transport_error "the agent disconnected before the response head"
    | Some (h : Upstream.head), (Upstream _ | Agent_gone) ->
        let complete =
          match finish with
          | Upstream (Ok ()) -> true
          | Upstream (Error _) | Agent_gone -> false
        in
        let body : Cassette.response_body =
          match x.request.shape with
          | Streamed ->
              Stream
                {
                  chunks =
                    List.map (fun (dt, piece) -> (dt, scrub st piece)) chunks;
                  complete;
                }
          | Whole_body ->
              Whole
                {
                  payload =
                    Store.payload st.run.store
                      (scrub st (String.concat "" (List.map snd chunks)));
                  complete;
                }
        in
        Response { status = h.status; headers = h.headers; body }
  in
  let (module A : Protocol.S) = x.adapter in
  {
    Cassette.seq = st.next_seq;
    t_ms = x.t_ms;
    channel = Model;
    protocol = A.name;
    meth = x.meth;
    path = x.path;
    query = x.query;
    key = x.request.key;
    occurrence = x.occurrence;
    canonical = x.request.canonical;
    request = Store.payload st.run.store x.scrubbed;
    outcome;
  }

(* Settle the ledger and record the exchange. Runs before the agent sees the
   end of the response, so findings are complete when the agent's call returns.
   An agent that went away leaves its reservation charged, like a cut-off
   upstream. *)
let finish_live st x ~record ~model ~date ~reservation ~head ~chunks finish =
  let actual =
    match (finish, head) with
    | Upstream (Error (Connect_failed _)), _ -> Some 0.
    | Upstream (Error (Failed_after_send _ | Timed_out | Truncated _)), _
    | Agent_gone, _
    | Upstream (Ok ()), None ->
        None
    | Upstream (Ok ()), Some h ->
        actual_cost x ~table:st.run.pricing ~model ~date h (List.map snd chunks)
  in
  Ledger.settle st.run.ledger reservation ~actual;
  locked st (fun () ->
      if Option.is_none actual then st.usage_gaps <- st.usage_gaps + 1;
      (match finish with
      | Upstream (Error e) ->
          st.transport_errors <- st.transport_errors + 1;
          add_error st (Upstream.describe_error e)
      | Agent_gone ->
          add_error st "the agent disconnected before the response ended"
      | Upstream (Ok ()) -> ());
      st.exchanges <- st.exchanges + 1;
      match st.run.cassette with
      | Some writer when record && not st.ended ->
          Store.append writer (record_live st x ~head ~chunks finish);
          st.next_seq <- st.next_seq + 1
      | Some _ | None -> ())

let refuse_spend st (refusal : Ledger.refusal) =
  match refusal with
  | Unknown_model model ->
      locked st (fun () -> st.cap_refusals <- refusal :: st.cap_refusals);
      refusal_response st ~status:402 ~type_:"bactrian_unknown_model"
        (Printf.sprintf "no price is known for model %S" model)
  | Unreadable { line; reason } ->
      locked st (fun () -> st.cap_refusals <- refusal :: st.cap_refusals);
      refusal_response st ~status:500 ~type_:"bactrian_ledger_unreadable"
        (Printf.sprintf "the spend ledger cannot be read (line %d: %s)" line
           reason)
  | Cap_reached { provider; cap; spent; requested } ->
      locked st (fun () -> st.cap_refusals <- refusal :: st.cap_refusals);
      refusal_response st ~status:402 ~type_:"bactrian_cap_reached"
        (Printf.sprintf
           "the %s spend cap of $%.2f would be exceeded ($%.4f spent or \
            reserved, $%.4f requested)"
           provider cap spent requested)

(* Forward to the provider. The upstream copy runs in its own fibre under its
   own switch, attached to the agent's connection: if the agent goes away, the
   connection's switch ends and cancels the copy. Chunks pass through a
   bounded stream, so a slow agent slows the upstream read. *)
let live t ~conn_sw st x ~record =
  let run = st.run in
  let model = x.model in
  let date = date_of (t.now ()) in
  match
    List.find_map
      (fun (p, k) -> if provider_equal p x.provider then Some k else None)
      run.keys
  with
  | None ->
      refusal_response st ~status:502 ~type_:"bactrian_no_key"
        ("no API key is configured for " ^ segment x.provider)
  | Some real_key -> (
      let reservation =
        match
          Reservation.worst_case ~table:run.pricing ~model ~date
            ~input_bytes:(String.length x.body)
            ~max_output:(max_output x.parsed) x.parsed
        with
        | None -> Error (Ledger.Unknown_model model)
        | Some usd ->
            Ledger.reserve run.ledger ~owner:run.run_id
              ~provider:(segment x.provider) ~model ~usd
      in
      match reservation with
      | Error refusal -> refuse_spend st refusal
      | Ok reservation -> (
          let request =
            {
              Upstream.meth = x.meth;
              url = upstream_url run x;
              headers = forward_headers x.headers x.provider real_key;
              body = x.body;
            }
          in
          let head_p, head_r = Eio.Promise.create () in
          let items = Eio.Stream.create 16 in
          Eio.Fiber.fork_daemon ~sw:conn_sw (fun () ->
              let head = ref None and chunks = ref [] and settled = ref false in
              let settle finish =
                settled := true;
                Eio.Cancel.protect (fun () ->
                    finish_live st x ~record ~model ~date ~reservation
                      ~head:!head ~chunks:(List.rev !chunks) finish)
              in
              (try
                 Eio.Switch.run (fun sw ->
                     let result =
                       t.call ~sw run.connector request
                         ~on_head:(fun h ->
                           head := Some h;
                           Eio.Promise.resolve head_r (Ok h))
                         ~on_chunk:(fun ~dt_ms piece ->
                           chunks := (dt_ms, piece) :: !chunks;
                           Eio.Stream.add items (Chunk piece))
                     in
                     settle (Upstream result);
                     match (result, !head) with
                     | Ok (), _ -> Eio.Stream.add items Finished
                     | Error _, Some _ -> Eio.Stream.add items Dropped
                     | Error e, None -> Eio.Promise.resolve head_r (Error e))
               with Eio.Cancel.Cancelled _ as ex ->
                 (* The agent's connection ended while the copy was running. *)
                 if not !settled then settle Agent_gone;
                 raise ex);
              `Stop_daemon);
          match Eio.Promise.await head_p with
          | Ok (h : Upstream.head) ->
              respond ~on_exn:(agent_failed st) ~status:h.status
                ~headers:h.headers (fun () -> Eio.Stream.take items)
          | Error e ->
              error_response ~status:502 ~type_:"bactrian_upstream_error"
                (Upstream.describe_error e)))

(* Replay *)

let serve_recorded t st ~timing (e : Cassette.exchange) =
  match e.outcome with
  | Transport_error reason ->
      error_response ~status:502 ~type_:"bactrian_upstream_error" reason
  | Response { status; headers; body = Whole { payload; complete } } -> (
      match Store.read_payload st.run.store payload with
      | Ok bytes ->
          let next = of_list [ bytes ] in
          respond ~on_exn:(agent_failed st) ~status ~headers (fun () ->
              match next () with
              | Finished when not complete -> Dropped
              | (Chunk _ | Finished | Dropped) as item -> item)
      | Error err ->
          error_response ~status:500 ~type_:"bactrian_internal_error"
            (Format.asprintf "%a" Store.pp_error err))
  | Response { status; headers; body = Stream { chunks; complete } } ->
      let rest = ref chunks in
      respond ~on_exn:(agent_failed st) ~status ~headers (fun () ->
          match !rest with
          | [] -> if complete then Finished else Dropped
          | (dt_ms, piece) :: tl ->
              rest := tl;
              (match timing with
              | Recorded -> t.sleep (float_of_int dt_ms /. 1000.)
              | Fast -> ());
              Chunk piece)

(* The matcher's decision, with its findings kept and a served exchange copied
   into this run's cassette. Shared with MCP. *)
let decide st (r : replaying) ~t_ms request =
  locked st (fun () ->
      let m, decision, found = Matcher.decide r.matcher request in
      r.matcher <- m;
      st.divergences <- List.rev_append found st.divergences;
      (match decision with
      | Matcher.Refuse d -> st.divergences <- d :: st.divergences
      | Serve { exchange; _ } -> (
          st.exchanges <- st.exchanges + 1;
          match st.run.cassette with
          | Some writer ->
              Store.append writer { exchange with seq = st.next_seq; t_ms };
              st.next_seq <- st.next_seq + 1
          | None -> ())
      | Go_live -> ());
      decision)

let replay t ~conn_sw st x (r : replaying) =
  match decide st r ~t_ms:x.t_ms x.request with
  | Serve { exchange; _ } -> serve_recorded t st ~timing:r.timing exchange
  | Go_live -> live t ~conn_sw st x ~record:true
  | Refuse d ->
      refusal_response st ~status:409 ~type_:"bactrian_replay_miss"
        ~extra:[ ("divergence", Replay_miss.json d) ]
        (Replay_miss.describe d)

(* Routing *)

let model_request t ~conn_sw st p ~meth ~uri ~path ~headers ~body =
  match Protocols.find ~meth ~path with
  | None ->
      refusal_response st ~status:404 ~type_:"bactrian_unknown_endpoint"
        (Printf.sprintf "%s %s is not a model endpoint bactrian knows" meth path)
  | Some ((module A : Protocol.S) as adapter) -> (
      let scrubbed = locked st (fun () -> scrub st body) in
      let parsed =
        match Canon.of_string scrubbed with
        | Error e -> Error (Format.asprintf "%a" Canon.pp_error e)
        | Ok parsed -> (
            match A.canonical_request ~path parsed with
            | Error e -> Error (Format.asprintf "%a" Protocol.pp_error e)
            | Ok canonical ->
                let model =
                  Option.value (Canon.str_member "model" canonical) ~default:""
                in
                Ok (parsed, model, canonical))
      in
      (* The one place [normalise] applies: keys, cassette and replay all use
         the masked canonical request. Pricing reads the model and the
         parsed body from before masking. *)
      let parsed =
        Result.map
          (fun (parsed, model, canonical) ->
            let masks, canonical = Mask.canon st.run.normalise canonical in
            locked st (fun () -> st.masks_fired <- st.masks_fired + masks);
            (parsed, model, canonical))
          parsed
      in
      match parsed with
      | Error reason ->
          refusal_response st ~status:400 ~type_:"bactrian_bad_request" reason
      | Ok (parsed, model, canonical) -> (
          let raw_query =
            match Uri.verbatim_query uri with Some "" -> None | q -> q
          in
          let query = normalised_query uri in
          let key = Matcher.key ~headers ~meth ~path ~query canonical in
          let shape : Matcher.shape =
            if
              A.streaming ~path
                ~query:(Option.value raw_query ~default:"")
                parsed
            then Streamed
            else Whole_body
          in
          let occurrence = locked st (fun () -> next_occurrence st key) in
          let x =
            {
              provider = p;
              adapter;
              meth;
              path;
              raw_query;
              query;
              headers;
              body;
              scrubbed;
              parsed;
              model;
              request = { channel = Model; key; canonical; shape };
              occurrence;
              t_ms = t_ms t st;
            }
          in
          match st.serving with
          | Live { record } -> live t ~conn_sw st x ~record
          | Replaying r -> replay t ~conn_sw st x r))

(* MCP *)

(* What MCP shares with the model routes of the same run (decision 0006). *)
let mcp_shared t st : Mcp_proxy.shared =
  {
    serving =
      (match st.serving with
      | Live { record } -> Live { record }
      | Replaying r -> Replaying { decide = decide st r });
    store = st.run.store;
    connector = st.run.connector;
    t_ms = (fun () -> t_ms t st);
    scrub = (fun text -> locked st (fun () -> scrub st text));
    mask =
      (fun canonical ->
        let masks, canonical = Mask.canon st.run.normalise canonical in
        locked st (fun () -> st.masks_fired <- st.masks_fired + masks);
        canonical);
    occurrence = (fun key -> locked st (fun () -> next_occurrence st key));
    exchange =
      (fun ~record e ->
        locked st (fun () ->
            st.exchanges <- st.exchanges + 1;
            match st.run.cassette with
            | Some writer when record && not st.ended ->
                Store.append writer { e with seq = st.next_seq };
                st.next_seq <- st.next_seq + 1
            | Some _ | None -> ()));
    failed =
      (fun message ->
        locked st (fun () ->
            st.transport_errors <- st.transport_errors + 1;
            add_error st message));
    note = note st;
  }

let mcp_response (r : Mcp_proxy.reply) =
  respond ~status:r.status ~headers:r.headers (of_list [ r.body ])

let mcp_request st ~meth ~uri ~segment ~path ~headers ~body =
  let mcp = Lazy.force st.mcp in
  match (segment, String.split_on_char '/' path) with
  | "mcp", [ ""; name ] -> mcp_response (Mcp_proxy.stdio mcp ~name body)
  | _, "" :: name :: rest ->
      mcp_response
        (Mcp_proxy.http mcp ~name ~meth
           ~rest:(String.concat "" (List.map (fun s -> "/" ^ s) rest))
           ~query:(match Uri.verbatim_query uri with Some "" -> None | q -> q)
           ~headers body)
  | _, _ ->
      refusal_response st ~status:404 ~type_:"bactrian_not_found"
        "expected /r/<token>/mcp/<server> or /r/<token>/mcp-http/<server>/..."

let route path =
  match String.split_on_char '/' path with
  | "" :: "r" :: token :: provider :: rest ->
      Some (token, provider, "/" ^ String.concat "/" rest)
  | _ -> None

let unattributed_response t ~status ~type_ message =
  t.unattributed <- t.unattributed + 1;
  error_response ~status ~type_ message

(* The placeholder key may arrive in the provider's key header and, for Gemini,
   also in the [key] query parameter. Each one present must be right, and at
   least one must be present. *)
let default_max_body = 64 * 1024 * 1024

type body = Body of string | Too_large

(* The body is read before any check, so a refused request leaves a keep-alive
   connection ready for the next one. *)
let read_body ~max_body body =
  match
    Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:max_body body)
  with
  | s -> Body s
  | exception Eio.Buf_read.Buffer_limit_exceeded -> Too_large

let handle ?only t ((conn_sw, _), _) request body : response =
  let headers = Http.Request.headers request in
  let uri = Uri.of_string (Http.Request.resource request) in
  (* A socket of one run serves no other: their tokens are unknown there. *)
  let find token =
    match only with
    | Some own when not (String.equal (token_key own) (token_key token)) -> None
    | Some _ | None -> Runs.find_opt t.runs (token_key token)
  in
  match (read_body ~max_body:t.max_body body, route (Uri.path uri)) with
  | Too_large, _ ->
      unattributed_response t ~status:413 ~type_:"bactrian_payload_too_large"
        (Printf.sprintf "the request body is over %d bytes" t.max_body)
  | Body _, None ->
      unattributed_response t ~status:404 ~type_:"bactrian_not_found"
        "expected /r/<token>/<provider>/<path>"
  | Body body, Some (token, provider, path) -> (
      match find token with
      | None ->
          unattributed_response t ~status:403 ~type_:"bactrian_forbidden"
            "unknown or ended run"
      | Some st -> (
          match provider_of_segment provider with
          | None
            when String.equal provider "mcp" || String.equal provider "mcp-http"
            ->
              mcp_request st ~uri ~segment:provider ~path
                ~meth:(Http.Method.to_string (Http.Request.meth request))
                ~headers:(Http.Header.to_list headers)
                ~body
          | None ->
              unattributed_response t ~status:404
                ~type_:"bactrian_unknown_provider"
                ("unknown provider " ^ provider)
          | Some p -> (
              let placeholder = "bactrian-" ^ st.run.token in
              let in_header =
                Option.map
                  (String.equal (key_value p placeholder))
                  (Http.Header.get headers (key_header p))
              and in_query =
                match p with
                | Gemini ->
                    Option.map (String.equal placeholder)
                      (Uri.get_query_param uri "key")
                | Openai | Anthropic | Openrouter -> None
              in
              match Option.to_list in_header @ Option.to_list in_query with
              | _ :: _ as checks when List.for_all Fun.id checks ->
                  let uri =
                    match p with
                    | Gemini -> Uri.remove_query_param uri "key"
                    | Openai | Anthropic | Openrouter -> uri
                  in
                  model_request t ~conn_sw st p
                    ~meth:(Http.Method.to_string (Http.Request.meth request))
                    ~uri ~path
                    ~headers:(Http.Header.to_list headers)
                    ~body
              | [] | _ :: _ ->
                  locked st (fun () -> st.refused_keys <- st.refused_keys + 1);
                  refusal_response st ~status:403 ~type_:"bactrian_forbidden"
                    ("missing or wrong placeholder key in " ^ key_header p))))

(* Exceptions that reach here could not be attributed to a run. *)
let on_error t = function
  | Upstream_dropped | Attributed _ -> ()
  | Eio.Io _ -> t.unattributed <- t.unattributed + 1
  | ex ->
      t.unattributed <- t.unattributed + 1;
      Printf.eprintf "bactrian model proxy: %s\n%!" (Printexc.to_string ex)

let serve ?only ~sw t sock =
  let server =
    Cohttp_eio.Server.make_expert
      ~callback:(fun conn request body -> handle ?only t conn request body)
      ()
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Cohttp_eio.Server.run sock server ~on_error:(on_error t))

let start ?(max_body = default_max_body) ~sw ~net ~clock ~process_mgr () =
  match
    Eio.Net.listen ~sw ~backlog:128 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  with
  | exception (Eio.Io _ as ex) -> Error (Printexc.to_string ex)
  | sock -> (
      match Eio.Net.listening_addr sock with
      | `Unix _ -> Error "the listener is not TCP"
      | `Tcp (_, port) ->
          let call ~sw connector request ~on_head ~on_chunk =
            Upstream.call ~sw ~net ~clock ~timeout:upstream_timeout connector
              request ~on_head ~on_chunk
          in
          let t =
            {
              port;
              unattributed = 0;
              runs = Runs.create 8;
              now = (fun () -> Eio.Time.now clock);
              sleep = Eio.Time.sleep clock;
              call;
              mcp_env =
                {
                  sw;
                  process_mgr :> Eio_unix.Process.mgr_ty Eio.Resource.t;
                  clock :> float Eio.Time.clock_ty Eio.Resource.t;
                  call;
                  answer_timeout = upstream_timeout;
                };
              max_body;
            }
          in
          serve ~sw t sock;
          Ok t)

let port t = t.port

let listen_unix t ~sw ~net ~token path =
  match Eio.Net.listen ~sw ~backlog:128 net (`Unix path) with
  | exception (Eio.Io _ as ex) -> Error (Printexc.to_string ex)
  | sock ->
      serve ~only:token ~sw t sock;
      Ok ()

let listen_tcp t ~sw ~net ~token =
  match
    Eio.Net.listen ~sw ~backlog:128 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  with
  | exception (Eio.Io _ as ex) -> Error (Printexc.to_string ex)
  | sock -> (
      match Eio.Net.listening_addr sock with
      | `Unix _ -> Error "the listener is not TCP"
      | `Tcp (_, port) ->
          serve ~only:token ~sw t sock;
          Ok port)

let unattributed t = t.unattributed

let register t run =
  if Runs.mem t.runs (token_key run.token) then
    invalid_arg "Model_proxy.register: token already registered";
  let serving =
    match run.mode with
    | Record -> Live { record = true }
    | Passthrough -> Live { record = false }
    | Replay { recording; matcher; allow_reorder; timing } ->
        Replaying
          {
            matcher =
              Matcher.start ~mode:matcher ~allow_reorder recording.exchanges;
            timing;
          }
  in
  let rec st =
    {
      run;
      serving;
      lock = Eio.Mutex.create ();
      redact = Redact.empty;
      seen = Counts.empty;
      next_seq = 0;
      divergences = [];
      exchanges = 0;
      transport_errors = 0;
      usage_gaps = 0;
      refused_keys = 0;
      masks_fired = 0;
      cap_refusals = [];
      errors = [];
      ended = false;
      mcp = lazy (Mcp_proxy.create t.mcp_env (mcp_shared t st) run.mcp);
    }
  in
  Runs.replace t.runs (token_key run.token) st

let unregister t ~token =
  match Runs.find_opt t.runs (token_key token) with
  | None -> invalid_arg "Model_proxy.unregister: unknown token"
  | Some st ->
      Runs.remove t.runs (token_key token);
      if Lazy.is_val st.mcp then Mcp_proxy.stop (Lazy.force st.mcp);
      locked st (fun () ->
          st.ended <- true;
          let unconsumed =
            match st.serving with
            | Replaying r -> Matcher.finish r.matcher
            | Live _ -> []
          in
          {
            divergences = List.rev_append st.divergences unconsumed;
            exchanges = st.exchanges;
            transport_errors = st.transport_errors;
            usage_gaps = st.usage_gaps;
            refused_keys = st.refused_keys;
            masks_fired = st.masks_fired;
            cap_refusals = List.rev st.cap_refusals;
            errors = List.rev st.errors;
          })

let base_url ?port t ~token p =
  Printf.sprintf "http://127.0.0.1:%d/r/%s/%s%s"
    (Option.value port ~default:t.port)
    token (segment p) (sdk_base p)

let mcp_url ?port t ~token =
  Printf.sprintf "http://127.0.0.1:%d/r/%s/mcp"
    (Option.value port ~default:t.port)
    token

let mcp_http_url ?port t ~token name =
  Printf.sprintf "http://127.0.0.1:%d/r/%s/mcp-http/%s"
    (Option.value port ~default:t.port)
    token name
