type mode = Strict | Until_divergence | Fuzzy of float
type shape = Streamed | Whole_body

type request = {
  channel : Cassette.channel;
  key : string;
  canonical : Canon.t;
  shape : shape;
}

let key ~meth ~path ~query canonical =
  let pair (k, v) = Canon.list [ Canon.string k; Canon.string v ] in
  let fields =
    [
      ("method", Canon.string meth);
      ("path", Canon.string path);
      ("query", Canon.list (List.map pair query));
      ("request", canonical);
    ]
  in
  match Canon.obj fields with
  | Ok v -> Canon.digest v
  | Error _ -> assert false (* the four field names above are distinct *)

type divergence =
  | Missing_request of {
      request : request;
      nearest : (Cassette.exchange * Diff.change list * float) option;
    }
  | Unconsumed of Cassette.exchange
  | Reordered of { request : request; expected : Cassette.exchange }
  | Shape_mismatch of { request : request; recorded : Cassette.exchange }

type decision =
  | Serve of { exchange : Cassette.exchange; similarity : float option }
  | Go_live
  | Refuse of divergence

let channel_rank : Cassette.channel -> int = function
  | Model -> 0
  | Mcp -> 1
  | Http -> 2

let channel_equal a b = Int.equal (channel_rank a) (channel_rank b)

module Seq_key = struct
  type t = int * int (* channel rank, seq *)

  let compare (c1, s1) (c2, s2) =
    match Int.compare c1 c2 with 0 -> Int.compare s1 s2 | n -> n
end

module Seq_map = Map.Make (Seq_key)

module Occ_key = struct
  type t = int * string * int (* channel rank, key, occurrence *)

  let compare (c1, k1, o1) (c2, k2, o2) =
    match Int.compare c1 c2 with
    | 0 -> ( match String.compare k1 k2 with 0 -> Int.compare o1 o2 | n -> n)
    | n -> n
end

module Occ_map = Map.Make (Occ_key)

module Count_key = struct
  type t = int * string

  let compare (c1, k1) (c2, k2) =
    match Int.compare c1 c2 with 0 -> String.compare k1 k2 | n -> n
end

module Count_map = Map.Make (Count_key)

type state = {
  mode : mode;
  allow_reorder : bool;
  by_occurrence : Cassette.exchange Occ_map.t;  (** Fixed after [start]. *)
  pending : Cassette.exchange Seq_map.t;  (** Unconsumed, by channel and seq. *)
  seen : int Count_map.t;  (** Requests so far per channel and key. *)
  switched : bool;
}

let seq_key (e : Cassette.exchange) = (channel_rank e.channel, e.seq)

let start ~mode ~allow_reorder exchanges =
  let add (occ, pending) (e : Cassette.exchange) =
    ( Occ_map.add (channel_rank e.channel, e.key, e.occurrence) e occ,
      Seq_map.add (seq_key e) e pending )
  in
  let by_occurrence, pending =
    List.fold_left add (Occ_map.empty, Seq_map.empty) exchanges
  in
  {
    mode;
    allow_reorder;
    by_occurrence;
    pending;
    seen = Count_map.empty;
    switched = false;
  }

let switched s = s.switched

(* Unconsumed exchanges of one channel in seq order; lazy, so taking the head is
   O(log n). *)
let channel_pending s channel =
  let rank = channel_rank channel in
  Seq_map.to_seq_from (rank, min_int) s.pending
  |> Seq.take_while (fun ((r, _), _) -> Int.equal r rank)
  |> Seq.map snd

let shape_of (e : Cassette.exchange) =
  match e.outcome with
  | Response { body = Stream _; _ } -> Some Streamed
  | Response { body = Whole _; _ } -> Some Whole_body
  | Transport_error _ -> None

let shape_equal a b =
  match (a, b) with
  | Streamed, Streamed | Whole_body, Whole_body -> true
  | Streamed, Whole_body | Whole_body, Streamed -> false

let consume s e = { s with pending = Seq_map.remove (seq_key e) s.pending }

let nearest s (r : request) =
  let best =
    Seq.fold_left
      (fun best e ->
        let sim = Diff.similarity e.Cassette.canonical r.canonical in
        match best with
        | Some (_, best_sim) when best_sim >= sim -> best
        | Some _ | None -> Some (e, sim))
      None
      (channel_pending s r.channel)
  in
  Option.map
    (fun (e, sim) -> (e, Diff.changes e.Cassette.canonical r.canonical, sim))
    best

let serve s (r : request) (e : Cassette.exchange) ~similarity ~findings =
  match shape_of e with
  | Some recorded when not (shape_equal recorded r.shape) ->
      (s, Refuse (Shape_mismatch { request = r; recorded = e }), findings)
  | Some _ | None -> (consume s e, Serve { exchange = e; similarity }, findings)

let miss s (r : request) =
  let nearest = nearest s r in
  let divergence = Missing_request { request = r; nearest } in
  match (s.mode, r.channel) with
  | Fuzzy threshold, _ -> (
      match nearest with
      | Some (e, _, sim) when sim >= threshold ->
          serve s r e ~similarity:(Some sim) ~findings:[]
      | Some _ | None -> (s, Refuse divergence, []))
  | Until_divergence, Model ->
      ({ s with switched = true }, Go_live, [ divergence ])
  | (Strict | Until_divergence), (Model | Mcp | Http) ->
      (s, Refuse divergence, [])

let matched s (r : request) (e : Cassette.exchange) =
  match r.channel with
  | Mcp -> serve s r e ~similarity:None ~findings:[]
  | Model | Http -> (
      match channel_pending s r.channel () with
      | Seq.Cons (first, _) when not (Int.equal first.Cassette.seq e.seq) -> (
          let reordered = Reordered { request = r; expected = first } in
          match s.mode with
          | Strict when not s.allow_reorder -> (s, Refuse reordered, [])
          | Strict | Until_divergence | Fuzzy _ ->
              serve s r e ~similarity:None ~findings:[ reordered ])
      | Seq.Cons _ | Seq.Nil -> serve s r e ~similarity:None ~findings:[])

let decide s (r : request) =
  let ck = (channel_rank r.channel, r.key) in
  let n = Option.value (Count_map.find_opt ck s.seen) ~default:0 in
  let s = { s with seen = Count_map.add ck (n + 1) s.seen } in
  if s.switched && channel_equal r.channel Model then (s, Go_live, [])
  else
    let exact =
      match Occ_map.find_opt (fst ck, r.key, n) s.by_occurrence with
      | Some e when Seq_map.mem (seq_key e) s.pending -> Some e
      | Some _ | None -> None
    in
    match exact with Some e -> matched s r e | None -> miss s r

let finish s =
  Seq_map.fold
    (fun _ (e : Cassette.exchange) acc ->
      match e.channel with
      | Model when s.switched -> acc
      | Model | Mcp | Http -> e :: acc)
    s.pending []
  |> List.sort (fun (a : Cassette.exchange) b -> Int.compare a.seq b.seq)
  |> List.map (fun e -> Unconsumed e)
