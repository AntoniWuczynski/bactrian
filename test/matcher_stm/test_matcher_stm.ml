(* Model-based test of Matcher.decide / finish against a naive reference model
   written from spec section 7 and the plan's matching rules. *)

open Bactrian_core

module type Config = sig
  val mode : Matcher.mode
  val allow_reorder : bool
  val recording : Cassette.exchange list
end

(* ---- total helpers ---- *)

let shape_of_exchange (e : Cassette.exchange) : Matcher.shape =
  match e.outcome with
  | Cassette.Response { body = Cassette.Stream _; _ } -> Matcher.Streamed
  | Cassette.Response { body = Cassette.Whole _; _ } -> Matcher.Whole_body
  | Cassette.Transport_error _ -> Matcher.Whole_body

let shape_equal (a : Matcher.shape) (b : Matcher.shape) =
  match (a, b) with
  | Matcher.Streamed, Matcher.Streamed | Matcher.Whole_body, Matcher.Whole_body
    ->
      true
  | Matcher.Streamed, Matcher.Whole_body | Matcher.Whole_body, Matcher.Streamed
    ->
      false

let channel_equal (a : Cassette.channel) (b : Cassette.channel) =
  match (a, b) with
  | Cassette.Model, Cassette.Model
  | Cassette.Mcp, Cassette.Mcp
  | Cassette.Http, Cassette.Http ->
      true
  | ( (Cassette.Model | Cassette.Mcp | Cassette.Http),
      (Cassette.Model | Cassette.Mcp | Cassette.Http) ) ->
      false

let is_mcp (c : Cassette.channel) =
  match c with Cassette.Mcp -> true | Cassette.Model | Cassette.Http -> false

let is_model (c : Cassette.channel) =
  match c with Cassette.Model -> true | Cassette.Mcp | Cassette.Http -> false

let channel_name (c : Cassette.channel) =
  match c with
  | Cassette.Model -> "Model"
  | Cassette.Mcp -> "Mcp"
  | Cassette.Http -> "Http"

let shape_name (s : Matcher.shape) =
  match s with Matcher.Streamed -> "Streamed" | Matcher.Whole_body -> "Whole"

let div_tag (d : Matcher.divergence) =
  match d with
  | Matcher.Missing_request _ -> "missing"
  | Matcher.Unconsumed _ -> "unconsumed"
  | Matcher.Reordered _ -> "reordered"
  | Matcher.Shape_mismatch _ -> "shape"

let tags ds = String.concat "," (List.sort String.compare (List.map div_tag ds))

(* ---- observation encoding shared by model and sut ---- *)

let obs_decision (d : Matcher.decision) (findings : Matcher.divergence list) =
  let head =
    match d with
    | Matcher.Serve { exchange; _ } -> "serve:" ^ string_of_int exchange.seq
    | Matcher.Go_live -> "live"
    | Matcher.Refuse dv -> "refuse:" ^ div_tag dv
  in
  head ^ "|" ^ tags findings

type obs_head = Serve_seq of int | Live | Refuse_tag of string

let obs_of_model head findings =
  let h =
    match head with
    | Serve_seq n -> "serve:" ^ string_of_int n
    | Live -> "live"
    | Refuse_tag t -> "refuse:" ^ t
  in
  h ^ "|" ^ String.concat "," (List.sort String.compare findings)

let obs_finish (ds : Matcher.divergence list) =
  "finish:"
  ^ String.concat ","
      (List.map
         (fun (d : Matcher.divergence) ->
           match d with
           | Matcher.Unconsumed e -> string_of_int e.seq
           | Matcher.Missing_request _ | Matcher.Reordered _
           | Matcher.Shape_mismatch _ ->
               "BAD")
         ds)

(* ---- reference model: lists and linear search ---- *)

type mstate = {
  consumed : int list;
  counts : ((string * string) * int) list; (* channel name, key *)
  switched : bool;
}

let mcount st ch key =
  match List.assoc_opt (channel_name ch, key) st.counts with
  | Some n -> n
  | None -> 0

let mbump st ch key =
  let n = mcount st ch key in
  let k = (channel_name ch, key) in
  { st with counts = (k, n + 1) :: List.remove_assoc k st.counts }

module Make (C : Config) = struct
  type cmd = Decide of Matcher.request | Finish
  type state = mstate
  type sut = Matcher.state ref

  let init_state = { consumed = []; counts = []; switched = false }

  let show_cmd = function
    | Finish -> "Finish"
    | Decide r ->
        Printf.sprintf "Decide(%s,%S,%s)" (channel_name r.channel) r.key
          (shape_name r.shape)

  let unconsumed st =
    List.filter
      (fun (e : Cassette.exchange) -> not (List.mem e.seq st.consumed))
      C.recording

  let lowest_unconsumed st ch =
    List.find_opt
      (fun (e : Cassette.exchange) -> channel_equal e.channel ch)
      (unconsumed st)

  let strict =
    match C.mode with
    | Matcher.Strict -> true
    | Matcher.Until_divergence | Matcher.Fuzzy _ -> false

  let until_div =
    match C.mode with
    | Matcher.Until_divergence -> true
    | Matcher.Strict | Matcher.Fuzzy _ -> false

  (* returns (head, findings, new model state) *)
  let model_decide st (r : Matcher.request) =
    let n = mcount st r.channel r.key in
    let st = mbump st r.channel r.key in
    if st.switched && is_model r.channel then (Live, [], st)
    else
      let found =
        List.find_opt
          (fun (e : Cassette.exchange) ->
            channel_equal e.channel r.channel
            && String.equal e.key r.key && e.occurrence = n
            && not (List.mem e.seq st.consumed))
          C.recording
      in
      match found with
      | Some e ->
          let reordered =
            (not (is_mcp r.channel))
            &&
            match lowest_unconsumed st r.channel with
            | Some low -> low.seq <> e.seq
            | None -> false
          in
          let findings = if reordered then [ "reordered" ] else [] in
          let wrong_shape = not (shape_equal (shape_of_exchange e) r.shape) in
          if reordered && strict && not C.allow_reorder then
            ( Refuse_tag "reordered",
              (if wrong_shape then [ "shape" ] else []),
              st )
          else if wrong_shape then (Refuse_tag "shape", findings, st)
          else
            ( Serve_seq e.seq,
              findings,
              { st with consumed = e.seq :: st.consumed } )
      | None ->
          if until_div && is_model r.channel then
            (Live, [ "missing" ], { st with switched = true })
          else (Refuse_tag "missing", [], st)

  let model_finish st =
    List.filter
      (fun (e : Cassette.exchange) -> not (st.switched && is_model e.channel))
      (unconsumed st)

  let next_state c st =
    match c with
    | Finish -> st
    | Decide r ->
        let _, _, st = model_decide st r in
        st

  let init_sut () =
    ref (Matcher.start ~mode:C.mode ~allow_reorder:C.allow_reorder C.recording)

  let cleanup _ = ()

  let run c sut =
    match c with
    | Finish -> STM.Res (STM.string, obs_finish (Matcher.finish !sut))
    | Decide r ->
        let s, d, f = Matcher.decide !sut r in
        sut := s;
        STM.Res (STM.string, obs_decision d f)

  let precond _ _ = true

  let postcond c st res =
    let expected =
      match c with
      | Finish ->
          "finish:"
          ^ String.concat ","
              (List.map
                 (fun (e : Cassette.exchange) -> string_of_int e.seq)
                 (model_finish st))
      | Decide r ->
          let h, f, _ = model_decide st r in
          obs_of_model h f
    in
    match res with
    | STM.Res ((STM.String, _), v) -> String.equal v expected
    | STM.Res _ -> false

  let arb_cmd st =
    let open QCheck.Gen in
    let rec_arr = Array.of_list C.recording in
    let of_exchange shape_flip (e : Cassette.exchange) =
      let sh = shape_of_exchange e in
      let sh =
        if shape_flip then
          match sh with
          | Matcher.Streamed -> Matcher.Whole_body
          | Matcher.Whole_body -> Matcher.Streamed
        else sh
      in
      ({
         channel = e.channel;
         key = e.key;
         canonical = Canon.string e.key;
         shape = sh;
       }
        : Matcher.request)
    in
    let from_rec =
      let* i = int_bound (Array.length rec_arr - 1) in
      let* flip = oneof_weighted [ (9, return false); (1, return true) ] in
      return (Decide (of_exchange flip rec_arr.(i)))
    in
    let next_expected =
      let* ch = oneof_list [ Cassette.Model; Cassette.Mcp; Cassette.Http ] in
      match lowest_unconsumed st ch with
      | Some e -> return (Decide (of_exchange false e))
      | None -> from_rec
    in
    let fresh =
      let* ch = oneof_list [ Cassette.Model; Cassette.Mcp; Cassette.Http ] in
      let* k = int_bound 2 in
      let* sh = oneof_list [ Matcher.Streamed; Matcher.Whole_body ] in
      let key = "fresh" ^ string_of_int k in
      return
        (Decide
           ({ channel = ch; key; canonical = Canon.string key; shape = sh }
             : Matcher.request))
    in
    let gen =
      oneof_weighted
        [ (5, next_expected); (4, from_rec); (2, fresh); (1, return Finish) ]
    in
    QCheck.make ~print:show_cmd gen
end

(* ---- fixed generated recording ---- *)

let gen_recording rand =
  let int_in lo hi = lo + Random.State.int rand (hi - lo + 1) in
  let n = int_in 3 12 in
  let nkeys = max 1 (n / 3) in
  let chans = [| Cassette.Model; Cassette.Mcp; Cassette.Http |] in
  let rec build i seq acc =
    if i >= n then List.rev acc
    else
      let channel = chans.(Random.State.int rand 3) in
      let key = "k" ^ string_of_int (Random.State.int rand nkeys) in
      let occurrence =
        List.length
          (List.filter
             (fun (e : Cassette.exchange) ->
               channel_equal e.channel channel && String.equal e.key key)
             acc)
      in
      let body =
        if Random.State.bool rand then
          Cassette.Whole { payload = Cassette.Inline "b"; complete = true }
        else Cassette.Stream { chunks = [ (0, "a") ]; complete = true }
      in
      let e : Cassette.exchange =
        {
          seq;
          t_ms = 0;
          channel;
          protocol = "x";
          meth = "POST";
          path = "/";
          query = [];
          key;
          occurrence;
          canonical = Canon.string key;
          request = Cassette.Inline "";
          outcome = Cassette.Response { status = 200; headers = []; body };
        }
      in
      build (i + 1) (seq + int_in 1 3) (e :: acc)
  in
  build 0 (int_in 0 5) []

let mode_name = function
  | Matcher.Strict -> "strict"
  | Matcher.Until_divergence -> "until-divergence"
  | Matcher.Fuzzy _ -> "fuzzy"

let make_test ~seed ~mode ~allow_reorder =
  let recording = gen_recording (Random.State.make [| seed |]) in
  let module S = Make (struct
    let mode = mode
    let allow_reorder = allow_reorder
    let recording = recording
  end) in
  let module T = STM_sequential.Make (S) in
  T.agree_test ~count:500
    ~name:
      (Printf.sprintf "matcher model: %s reorder=%b seed=%d" (mode_name mode)
         allow_reorder seed)

let () =
  let base =
    Random.self_init ();
    Random.int 60000
  in
  let tests =
    List.concat_map
      (fun (mode, allow_reorder) ->
        List.map
          (fun seed -> make_test ~seed ~mode ~allow_reorder)
          [ base; base + 1; base + 2 ])
      [
        (Matcher.Strict, false);
        (Matcher.Strict, true);
        (Matcher.Until_divergence, false);
        (Matcher.Until_divergence, true);
      ]
  in
  exit (QCheck_base_runner.run_tests ~verbose:true tests)
