open Bactrian_core
module Smap = Map.Make (String)
module Imap = Map.Make (Int)

type open_res = { o_provider : string; o_usd : float }

type t = {
  path : Eio.Fs.dir_ty Eio.Path.t;
  caps : float Smap.t;
  mutex : Eio.Mutex.t;
  mutable charged : float Smap.t;
  mutable next_id : int;
}

type reservation = {
  id : int;
  provider : string;
  model : string;
  usd : float;
  mutable settled : bool;
}

type refusal =
  | Cap_reached of {
      provider : string;
      cap : float;
      spent : float;
      requested : float;
    }
  | Unknown_model of string

exception Corrupt of { line : int; reason : string }

let get_charged m provider = Option.value ~default:0. (Smap.find_opt provider m)
let add m provider usd = Smap.add provider (get_charged m provider +. usd) m

let now () =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let number key v =
  match Canon.member key v with
  | Some (Canon.Float f) -> Some f
  | Some (Canon.Int i) -> Some (Float.of_int i)
  | Some
      ( Canon.Null | Canon.Bool _ | Canon.Big_int _ | Canon.String _
      | Canon.List _ | Canon.Object _ )
  | None ->
      None

type event =
  | Legacy of { provider : string; usd : float }
  | Reserved of { id : int; provider : string; usd : float }
  | Ended of { id : int; usd : float }

let decode_line v =
  let ( let* ) = Result.bind in
  let need what = function Some x -> Ok x | None -> Error what in
  let* provider = need "missing provider" (Canon.str_member "provider" v) in
  match Canon.str_member "event" v with
  | None ->
      let* usd = need "missing usd" (number "usd" v) in
      Ok (Legacy { provider; usd })
  | Some "reserve" ->
      let* id = need "missing id" (Canon.int_member "id" v) in
      let* usd = need "missing reserved_usd" (number "reserved_usd" v) in
      Ok (Reserved { id; provider; usd })
  | Some ("settle" | "gap") ->
      let* id = need "missing id" (Canon.int_member "id" v) in
      let* usd = need "missing usd" (number "usd" v) in
      Ok (Ended { id; usd })
  | Some other -> Error ("unknown event " ^ other)

(* Replays the file: a reservation counts at its reserved amount until a
   settle line for its id replaces it. *)
let replay lines =
  let step (n, legacy, reserved, ended, max_id) line =
    let fail reason = raise (Corrupt { line = n; reason }) in
    if String.equal (String.trim line) "" then
      (n + 1, legacy, reserved, ended, max_id)
    else
      match Canon.of_string line with
      | Error _ -> fail "not JSON"
      | Ok v -> (
          match decode_line v with
          | Error reason -> fail reason
          | Ok (Legacy { provider; usd }) ->
              (n + 1, add legacy provider usd, reserved, ended, max_id)
          | Ok (Reserved { id; provider; usd }) ->
              ( n + 1,
                legacy,
                Imap.add id { o_provider = provider; o_usd = usd } reserved,
                ended,
                max id max_id )
          | Ok (Ended { id; usd }) ->
              (n + 1, legacy, reserved, Imap.add id usd ended, max id max_id))
  in
  let _, legacy, reserved, ended, max_id =
    List.fold_left step (1, Smap.empty, Imap.empty, Imap.empty, -1) lines
  in
  let charged =
    Imap.fold
      (fun id { o_provider; o_usd } acc ->
        add acc o_provider
          (Option.value ~default:o_usd (Imap.find_opt id ended)))
      reserved legacy
  in
  (charged, max_id + 1)

let create ~path ~caps =
  (match Eio.Path.split path with
  | Some (parent, _) -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 parent
  | None -> ());
  let charged, next_id =
    match Eio.Path.kind ~follow:true path with
    | `Not_found -> (Smap.empty, 0)
    | `Regular_file | `Directory | `Symbolic_link | `Fifo | `Character_special
    | `Block_device | `Socket | `Unknown ->
        replay (String.split_on_char '\n' (Eio.Path.load path))
  in
  {
    path;
    caps = List.fold_left (fun m (k, v) -> Smap.add k v m) Smap.empty caps;
    mutex = Eio.Mutex.create ();
    charged;
    next_id;
  }

let append t fields =
  match Canon.obj (("date", Canon.string (now ())) :: fields) with
  | Error _ -> invalid_arg "Ledger: non-finite amount in event"
  | Ok v ->
      Eio.Path.save ~append:true ~create:(`If_missing 0o600) t.path
        (Canon.to_string v ^ "\n")

(* Callers pass finite amounts only, so the [Error] case cannot happen. *)
let money usd = Result.value (Canon.float usd) ~default:Canon.null

let reserve t ~provider ~model ~usd =
  Eio.Mutex.use_rw ~protect:true t.mutex @@ fun () ->
  let cap = Option.value ~default:0. (Smap.find_opt provider t.caps) in
  let spent = get_charged t.charged provider in
  if Float.is_finite usd && usd >= 0. && spent +. usd <= cap then begin
    let id = t.next_id in
    append t
      [
        ("event", Canon.string "reserve");
        ("id", Canon.int id);
        ("model", Canon.string model);
        ("provider", Canon.string provider);
        ("reserved_usd", money usd);
      ];
    t.next_id <- id + 1;
    t.charged <- add t.charged provider usd;
    Ok { id; provider; model; usd; settled = false }
  end
  else Error (Cap_reached { provider; cap; spent; requested = usd })

let settle t r ~actual =
  Eio.Mutex.use_rw ~protect:true t.mutex @@ fun () ->
  if not r.settled then begin
    let final, event =
      match actual with
      | Some a when Float.is_finite a && a >= 0. -> (a, "settle")
      | Some _ | None -> (r.usd, "gap")
    in
    append t
      [
        ("event", Canon.string event);
        ("id", Canon.int r.id);
        ("model", Canon.string r.model);
        ("provider", Canon.string r.provider);
        ("usd", money final);
      ];
    r.settled <- true;
    t.charged <- add t.charged r.provider (final -. r.usd)
  end

let spent t ~provider = get_charged t.charged provider
