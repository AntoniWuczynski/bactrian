open Bactrian_core
module Smap = Map.Make (String)
module Imap = Map.Make (Int)

type open_res = { o_owner : string; o_provider : string; o_usd : float }

type t = {
  path : string;
  caps : float Smap.t;
  mutex : Eio.Mutex.t;
  mutable charged : float Smap.t;
  mutable charged_by : float Smap.t;
  mutable outstanding : open_res Imap.t;
      (** Reservations of any process with no end line read yet. *)
  mutable next_id : int;
  mutable offset : int;  (** Bytes of the file read so far, whole lines. *)
  mutable lines : int;  (** Lines read so far. *)
  mutable unreadable : (int * string) option;
      (** A line another process appended that could not be read. *)
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
  | Unreadable of { line : int; reason : string }

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
  | Reserved of { id : int; owner : string; provider : string; usd : float }
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
      let owner = Option.value ~default:"" (Canon.str_member "owner" v) in
      Ok (Reserved { id; owner; provider; usd })
  | Some ("settle" | "gap") ->
      let* id = need "missing id" (Canon.int_member "id" v) in
      let* usd = need "missing usd" (number "usd" v) in
      Ok (Ended { id; usd })
  | Some other -> Error ("unknown event " ^ other)

let event_of_line line =
  match Canon.of_string line with
  | Error _ -> Error "not JSON"
  | Ok v -> decode_line v

(* Replays the whole file: a reservation counts at its reserved amount until a
   settle line for its id replaces it. A later line with an id seen before
   replaces the earlier one, as files written before the lock could hold
   colliding ids. *)
let replay lines =
  let step (n, legacy, reserved, ended, max_id) line =
    if String.equal (String.trim line) "" then
      (n + 1, legacy, reserved, ended, max_id)
    else
      match event_of_line line with
      | Error reason -> raise (Corrupt { line = n; reason })
      | Ok (Legacy { provider; usd }) ->
          (n + 1, add legacy provider usd, reserved, ended, max_id)
      | Ok (Reserved { id; owner; provider; usd }) ->
          ( n + 1,
            legacy,
            Imap.add id
              { o_owner = owner; o_provider = provider; o_usd = usd }
              reserved,
            ended,
            max id max_id )
      | Ok (Ended { id; usd }) ->
          (n + 1, legacy, reserved, Imap.add id usd ended, max id max_id)
  in
  let _, legacy, reserved, ended, max_id =
    List.fold_left step (1, Smap.empty, Imap.empty, Imap.empty, -1) lines
  in
  let final id o_usd = Option.value ~default:o_usd (Imap.find_opt id ended) in
  let charged =
    Imap.fold
      (fun id { o_provider; o_usd; o_owner = _ } acc ->
        add acc o_provider (final id o_usd))
      reserved legacy
  in
  let charged_by =
    Imap.fold
      (fun id { o_owner; o_usd; o_provider = _ } acc ->
        add acc o_owner (final id o_usd))
      reserved Smap.empty
  in
  let outstanding =
    Imap.filter (fun id _ -> not (Imap.mem id ended)) reserved
  in
  (charged, charged_by, outstanding, max_id + 1)

(* Every read and write happens with the file open and locked: [lockf] keeps
   other processes out, and the calls in between are plain system calls that
   never yield, so no other fibre of this process runs while the lock is held
   (a [lockf] lock belongs to the process, and closing any descriptor of the
   file would drop it). Waiting for another process blocks this domain, for as
   long as that process takes to read and append a line. *)
let io_error e fn arg =
  Eio.Exn.create (Eio.Exn.X (Eio_unix.Unix_error (e, fn, arg)))

let with_locked path f =
  match Unix.openfile path [ O_RDWR; O_APPEND; O_CREAT; O_CLOEXEC ] 0o600 with
  | exception Unix.Unix_error (e, fn, arg) -> raise (io_error e fn arg)
  | fd -> (
      Fun.protect ~finally:(fun () -> Unix.close fd) @@ fun () ->
      match
        ignore (Unix.lseek fd 0 SEEK_SET : int);
        Unix.lockf fd F_LOCK 0
      with
      | exception Unix.Unix_error (e, fn, arg) -> raise (io_error e fn arg)
      | () -> f fd)

(* Everything from [offset] to the end of the file. *)
let read_from fd offset =
  let buf = Buffer.create 4096 and chunk = Bytes.create 65536 in
  let rec go () =
    match Unix.read fd chunk 0 (Bytes.length chunk) with
    | 0 -> Buffer.contents buf
    | n ->
        Buffer.add_subbytes buf chunk 0 n;
        go ()
    | exception Unix.Unix_error (EINTR, _, _) -> go ()
    | exception Unix.Unix_error (e, fn, arg) -> raise (io_error e fn arg)
  in
  match Unix.lseek fd offset SEEK_SET with
  | exception Unix.Unix_error (e, fn, arg) -> raise (io_error e fn arg)
  | _ -> go ()

let write_all fd s =
  let rec go off =
    if off < String.length s then
      match Unix.write_substring fd s off (String.length s - off) with
      | n -> go (off + n)
      | exception Unix.Unix_error (EINTR, _, _) -> go off
      | exception Unix.Unix_error (e, fn, arg) -> raise (io_error e fn arg)
  in
  go 0

let create ~path ~caps =
  (match Eio.Path.split path with
  | Some (parent, _) -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 parent
  | None -> ());
  let native = Eio.Path.native_exn path in
  let text = with_locked native (fun fd -> read_from fd 0) in
  let charged, charged_by, outstanding, next_id =
    replay (String.split_on_char '\n' text)
  in
  {
    path = native;
    caps = List.fold_left (fun m (k, v) -> Smap.add k v m) Smap.empty caps;
    mutex = Eio.Mutex.create ();
    charged;
    charged_by;
    outstanding;
    next_id;
    offset = String.length text;
    lines = String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 text;
    unreadable = None;
  }

let apply t = function
  | Legacy { provider; usd } -> t.charged <- add t.charged provider usd
  | Reserved { id; owner; provider; usd } ->
      t.charged <- add t.charged provider usd;
      t.charged_by <- add t.charged_by owner usd;
      t.outstanding <-
        Imap.add id
          { o_owner = owner; o_provider = provider; o_usd = usd }
          t.outstanding;
      t.next_id <- max t.next_id (id + 1)
  | Ended { id; usd } -> (
      t.next_id <- max t.next_id (id + 1);
      match Imap.find_opt id t.outstanding with
      | None -> ()
      | Some o ->
          t.charged <- add t.charged o.o_provider (usd -. o.o_usd);
          t.charged_by <- add t.charged_by o.o_owner (usd -. o.o_usd);
          t.outstanding <- Imap.remove id t.outstanding)

(* Takes in the whole lines other processes appended since the last read. A
   line still being written has no newline yet and waits for the next read. *)
let catch_up t fd =
  if Option.is_none t.unreadable then
    let tail = read_from fd t.offset in
    let complete =
      match String.rindex_opt tail '\n' with
      | None -> []
      | Some last ->
          String.split_on_char '\n' (String.sub tail 0 last)
          |> List.map (fun l -> l ^ "\n")
    in
    List.iter
      (fun line ->
        if Option.is_none t.unreadable then begin
          let text = String.trim line in
          (if not (String.equal text "") then
             match event_of_line text with
             | Ok event -> apply t event
             | Error reason -> t.unreadable <- Some (t.lines + 1, reason));
          if Option.is_none t.unreadable then begin
            t.offset <- t.offset + String.length line;
            t.lines <- t.lines + 1
          end
        end)
      complete

let append t fd fields =
  match Canon.obj (("date", Canon.string (now ())) :: fields) with
  | Error _ -> invalid_arg "Ledger: non-finite amount in event"
  | Ok v ->
      let line = Canon.to_string v ^ "\n" in
      write_all fd line;
      t.offset <- t.offset + String.length line;
      t.lines <- t.lines + 1

(* Callers pass finite amounts only, so the [Error] case cannot happen. *)
let money usd = Result.value (Canon.float usd) ~default:Canon.null

let reserve t ~owner ~provider ~model ~usd =
  Eio.Mutex.use_rw ~protect:true t.mutex @@ fun () ->
  with_locked t.path @@ fun fd ->
  catch_up t fd;
  match t.unreadable with
  | Some (line, reason) -> Error (Unreadable { line; reason })
  | None ->
      let cap = Option.value ~default:0. (Smap.find_opt provider t.caps) in
      let spent = get_charged t.charged provider in
      if Float.is_finite usd && usd >= 0. && spent +. usd <= cap then begin
        let id = t.next_id in
        append t fd
          [
            ("event", Canon.string "reserve");
            ("id", Canon.int id);
            ("model", Canon.string model);
            ("owner", Canon.string owner);
            ("provider", Canon.string provider);
            ("reserved_usd", money usd);
          ];
        apply t (Reserved { id; owner; provider; usd });
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
    with_locked t.path (fun fd ->
        catch_up t fd;
        append t fd
          [
            ("event", Canon.string event);
            ("id", Canon.int r.id);
            ("model", Canon.string r.model);
            ("provider", Canon.string r.provider);
            ("usd", money final);
          ]);
    r.settled <- true;
    apply t (Ended { id = r.id; usd = final })
  end

let spent t ~provider = get_charged t.charged provider
let spent_by t ~owner = get_charged t.charged_by owner
