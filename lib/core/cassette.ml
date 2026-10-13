(* Wire format: one flat JSON object per line. The envelope keys "v" and "type"
   sit beside the line's own fields. Each line type has a private "wire" record
   deriving yojson (strict mode, so unknown fields are rejected). A response is
   flattened into the exchange line as "status", "headers" and "body"; a
   transport error is the "error" field alone. Exactly one of the two shapes is
   accepted. The final object goes through [Canon] so key order is stable. *)

let schema_version = 1

type channel = Model | Mcp | Http
type payload = Inline of string | Blob of string
type response_body = Whole of payload | Stream of (int * string) list

type response = {
  status : int;
  headers : (string * string) list;
  body : response_body;
}

type exchange = {
  seq : int;
  t_ms : int;
  channel : channel;
  protocol : string;
  meth : string;
  path : string;
  query : (string * string) list;
  key : string;
  occurrence : int;
  canonical : Canon.t;
  request : payload;
  outcome : outcome;
}

and outcome = Response of response | Transport_error of string

type header = {
  run_id : string;
  scenario : string;
  variant : string;
  bactrian_version : string;
  recorded_at : string;
  sim_now : string;
  clock : Trace.clock;
}

type t = { header : header; exchanges : exchange list; complete : bool }

type error =
  | Invalid_json of { line : int; reason : string }
  | Unsupported_version of int
  | Bad_line of { line : int; reason : string }
  | Missing_header
  | Out_of_order of { line : int }

(* ---- Field codecs picked up by the ppx by name ---- *)

let enum_of_yojson ~what table = function
  | `String s -> (
      match List.assoc_opt s table with
      | Some v -> Ok v
      | None -> Error (Printf.sprintf "unknown %s %S" what s))
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `List _ | `Assoc _ ->
      Error (what ^ " must be a string")

let channel_to_yojson = function
  | Model -> `String "model"
  | Mcp -> `String "mcp"
  | Http -> `String "http"

let channel_of_yojson =
  enum_of_yojson ~what:"channel"
    [ ("model", Model); ("mcp", Mcp); ("http", Http) ]

module Clock = struct
  type t = Trace.clock

  let to_yojson : t -> Yojson.Safe.t = function
    | Enforced -> `String "enforced"
    | Contract_only -> `String "contract_only"

  let of_yojson =
    enum_of_yojson ~what:"clock"
      [ ("enforced", Trace.Enforced); ("contract_only", Trace.Contract_only) ]
end

(* A one-field object whose key names the variant. *)
let tagged_of_yojson ~what table = function
  | `Assoc [ (tag, v) ] -> (
      match List.assoc_opt tag table with
      | Some decode -> decode v
      | None -> Error (Printf.sprintf "unknown %s %S" what tag))
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _
  | `Assoc _ ->
      Error (what ^ " must be an object with exactly one field")

let payload_to_yojson = function
  | Inline s -> `Assoc [ ("inline", `String s) ]
  | Blob s -> `Assoc [ ("blob", `String s) ]

let payload_of_yojson =
  let text make = function
    | `String s -> Ok (make s)
    | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `List _ | `Assoc _ ->
        Error "payload content must be a string"
  in
  tagged_of_yojson ~what:"payload"
    [ ("inline", text (fun s -> Inline s)); ("blob", text (fun s -> Blob s)) ]

type chunks = (int * string) list [@@deriving yojson]

let response_body_to_yojson = function
  | Whole p -> `Assoc [ ("whole", payload_to_yojson p) ]
  | Stream chunks -> `Assoc [ ("stream", chunks_to_yojson chunks) ]

let response_body_of_yojson =
  tagged_of_yojson ~what:"body"
    [
      ("whole", fun j -> Result.map (fun p -> Whole p) (payload_of_yojson j));
      ("stream", fun j -> Result.map (fun c -> Stream c) (chunks_of_yojson j));
    ]

module Canon_json = struct
  type t = Canon.t

  let to_yojson = Canon.to_yojson

  let of_yojson j =
    Result.map_error (Format.asprintf "%a" Canon.pp_error) (Canon.of_yojson j)
end

(* ---- Wire records ---- *)

type header_w = {
  run_id : string;
  scenario : string;
  variant : string;
  bactrian_version : string;
  recorded_at : string;
  sim_now : string;
  clock : Clock.t;
}
[@@deriving yojson]

type exchange_w = {
  seq : int;
  t_ms : int;
  channel : channel;
  protocol : string;
  meth : string; [@key "method"]
  path : string;
  query : (string * string) list;
  key : string;
  occurrence : int;
  canonical : Canon_json.t;
  request : payload;
  status : int option; [@default None]
  headers : (string * string) list option; [@default None]
  body : response_body option; [@default None]
  error : string option; [@default None]
}
[@@deriving yojson]

(* ---- Encoding ---- *)

let line_of ty (payload : Yojson.Safe.t) =
  let fields =
    match payload with
    | `Assoc fields -> fields
    | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
        []
  in
  let json =
    `Assoc (("v", `Int schema_version) :: ("type", `String ty) :: fields)
  in
  match Canon.of_yojson json with
  | Ok c -> Canon.to_string c
  | Error _ ->
      (* Only reachable if a payload held a non-finite float, which no field
         can: fall back to the uncanonicalised text rather than lose the
         line. *)
      Yojson.Safe.to_string json

let header_line (h : header) =
  line_of "header"
    (header_w_to_yojson
       {
         run_id = h.run_id;
         scenario = h.scenario;
         variant = h.variant;
         bactrian_version = h.bactrian_version;
         recorded_at = h.recorded_at;
         sim_now = h.sim_now;
         clock = h.clock;
       })

let exchange_line (e : exchange) =
  let status, headers, body, error =
    match e.outcome with
    | Response { status; headers; body } ->
        (Some status, Some headers, Some body, None)
    | Transport_error msg -> (None, None, None, Some msg)
  in
  line_of "exchange"
    (exchange_w_to_yojson
       {
         seq = e.seq;
         t_ms = e.t_ms;
         channel = e.channel;
         protocol = e.protocol;
         meth = e.meth;
         path = e.path;
         query = e.query;
         key = e.key;
         occurrence = e.occurrence;
         canonical = e.canonical;
         request = e.request;
         status;
         headers;
         body;
         error;
       })

let end_line = line_of "end" (`Assoc [])

(* ---- Decoding ---- *)

type parsed = Header of header | Exchange of exchange | End
type line_error = Json of string | Version of int | Bad of string

let exchange_of_wire (w : exchange_w) =
  let outcome =
    match (w.status, w.headers, w.body, w.error) with
    | Some status, Some headers, Some body, None ->
        Ok (Response { status; headers; body })
    | None, None, None, Some msg -> Ok (Transport_error msg)
    | _ ->
        Error
          "an exchange carries either status, headers and body, or error alone"
  in
  Result.map
    (fun outcome ->
      {
        seq = w.seq;
        t_ms = w.t_ms;
        channel = w.channel;
        protocol = w.protocol;
        meth = w.meth;
        path = w.path;
        query = w.query;
        key = w.key;
        occurrence = w.occurrence;
        canonical = w.canonical;
        request = w.request;
        outcome;
      })
    outcome

let take key fields =
  let found, rest = List.partition (fun (k, _) -> String.equal k key) fields in
  (Option.map snd (List.nth_opt found 0), rest)

let decode_line line =
  let open Result.Syntax in
  let* doc =
    Result.map_error
      (fun e -> Json (Format.asprintf "%a" Canon.pp_error e))
      (Canon.of_string line)
  in
  match Canon.to_yojson doc with
  | `Assoc fields -> (
      let v, fields = take "v" fields in
      let* () =
        match v with
        | Some (`Int 1) -> Ok ()
        | Some (`Int n) -> Error (Version n)
        | Some _ -> Error (Bad "v must be an integer")
        | None -> Error (Bad "missing v")
      in
      let ty, fields = take "type" fields in
      let body = `Assoc fields in
      let bad r = Result.map_error (fun m -> Bad m) r in
      match ty with
      | Some (`String "header") ->
          bad
            (Result.map
               (fun ({
                       run_id;
                       scenario;
                       variant;
                       bactrian_version;
                       recorded_at;
                       sim_now;
                       clock;
                     } :
                      header_w) ->
                 Header
                   {
                     run_id;
                     scenario;
                     variant;
                     bactrian_version;
                     recorded_at;
                     sim_now;
                     clock;
                   })
               (header_w_of_yojson body))
      | Some (`String "exchange") ->
          let* w = bad (exchange_w_of_yojson body) in
          bad (Result.map (fun e -> Exchange e) (exchange_of_wire w))
      | Some (`String "end") -> (
          match fields with
          | [] -> Ok End
          | _ :: _ -> Error (Bad "end carries no fields"))
      | Some (`String other) ->
          Error (Bad (Printf.sprintf "unknown type %S" other))
      | Some _ -> Error (Bad "type must be a string")
      | None -> Error (Bad "missing type"))
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
      Error (Bad "line must be a JSON object")

let located line = function
  | Json reason -> Invalid_json { line; reason }
  | Version n -> Unsupported_version n
  | Bad reason -> Bad_line { line; reason }

let rec drop_trailing_empty = function
  | "" :: rest -> drop_trailing_empty rest
  | lines -> lines

let of_lines lines =
  let open Result.Syntax in
  let lines = List.rev (drop_trailing_empty (List.rev lines)) in
  let decode n line = Result.map_error (located n) (decode_line line) in
  let rec go n header acc last_seq = function
    | [] -> Ok { header; exchanges = List.rev acc; complete = false }
    | line :: rest -> (
        let* parsed = decode n line in
        match parsed with
        | Header _ -> Error (Bad_line { line = n; reason = "duplicate header" })
        | End -> (
            match rest with
            | [] -> Ok { header; exchanges = List.rev acc; complete = true }
            | _ :: _ ->
                Error
                  (Bad_line { line = n + 1; reason = "line after the end line" })
            )
        | Exchange e -> (
            match last_seq with
            | Some s when e.seq <= s -> Error (Out_of_order { line = n })
            | Some _ | None -> go (n + 1) header (e :: acc) (Some e.seq) rest))
  in
  match lines with
  | [] -> Error Missing_header
  | first :: rest -> (
      let* parsed = decode 1 first in
      match parsed with
      | Header header -> go 2 header [] None rest
      | Exchange _ | End -> Error Missing_header)

(* ---- Equality and printing ---- *)

let channel_equal a b =
  match (a, b) with
  | Model, Model | Mcp, Mcp | Http, Http -> true
  | Model, (Mcp | Http) | Mcp, (Model | Http) | Http, (Model | Mcp) -> false

let payload_equal a b =
  match (a, b) with
  | Inline x, Inline y | Blob x, Blob y -> String.equal x y
  | Inline _, Blob _ | Blob _, Inline _ -> false

let pair_equal eq_a eq_b (a1, b1) (a2, b2) = eq_a a1 a2 && eq_b b1 b2
let string_pairs_equal = List.equal (pair_equal String.equal String.equal)

let body_equal a b =
  match (a, b) with
  | Whole x, Whole y -> payload_equal x y
  | Stream x, Stream y -> List.equal (pair_equal Int.equal String.equal) x y
  | Whole _, Stream _ | Stream _, Whole _ -> false

let outcome_equal a b =
  match (a, b) with
  | Response x, Response y ->
      Int.equal x.status y.status
      && string_pairs_equal x.headers y.headers
      && body_equal x.body y.body
  | Transport_error x, Transport_error y -> String.equal x y
  | Response _, Transport_error _ | Transport_error _, Response _ -> false

let equal_exchange (a : exchange) (b : exchange) =
  Int.equal a.seq b.seq && Int.equal a.t_ms b.t_ms
  && channel_equal a.channel b.channel
  && String.equal a.protocol b.protocol
  && String.equal a.meth b.meth && String.equal a.path b.path
  && string_pairs_equal a.query b.query
  && String.equal a.key b.key
  && Int.equal a.occurrence b.occurrence
  && Canon.equal a.canonical b.canonical
  && payload_equal a.request b.request
  && outcome_equal a.outcome b.outcome

let pp_error ppf = function
  | Invalid_json { line; reason } ->
      Format.fprintf ppf "line %d: invalid JSON: %s" line reason
  | Unsupported_version n ->
      Format.fprintf ppf "unsupported cassette version %d" n
  | Bad_line { line; reason } -> Format.fprintf ppf "line %d: %s" line reason
  | Missing_header -> Format.fprintf ppf "the cassette has no header line"
  | Out_of_order { line } ->
      Format.fprintf ppf "line %d: seq is not strictly increasing" line
