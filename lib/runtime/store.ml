open Bactrian_core

type t = { root : Eio.Fs.dir_ty Eio.Path.t }

type error =
  | Invalid_id of string
  | Invalid_digest of string
  | No_such_cassette of string
  | No_such_blob of string
  | Blob_corrupt of string
  | Bad_cassette of Cassette.error

let create ~root = { root }

let pp_error ppf = function
  | Invalid_id s -> Format.fprintf ppf "invalid run id %S" s
  | Invalid_digest s -> Format.fprintf ppf "invalid blob digest %S" s
  | No_such_cassette s -> Format.fprintf ppf "no cassette for run %S" s
  | No_such_blob s -> Format.fprintf ppf "no blob %s" s
  | Blob_corrupt s -> Format.fprintf ppf "blob %s does not match its digest" s
  | Bad_cassette e -> Cassette.pp_error ppf e

let counter = ref 0

let write_atomic ?(before_rename = Fun.id) path data =
  let dir, leaf =
    match Eio.Path.split path with
    | Some p -> p
    | None -> invalid_arg "Store.write_atomic: path has no parent"
  in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  incr counter;
  let tmp =
    Eio.Path.(
      dir / Printf.sprintf ".%s.%d.%d.tmp" leaf (Unix.getpid ()) !counter)
  in
  (try
     Eio.Path.with_open_out ~create:(`Exclusive 0o600) tmp (fun f ->
         Eio.Flow.copy_string data f;
         Eio.File.sync f)
   with Eio.Io _ as e ->
     Eio.Path.unlink ~missing_ok:true tmp;
     raise e);
  before_rename ();
  Eio.Path.rename tmp path

let valid_id s =
  (not (String.equal s ""))
  && (not (String.equal s "."))
  && (not (String.equal s ".."))
  && not (String.contains s '/' || String.contains s '\000')

let run_dir t ~run_id =
  if valid_id run_id then Ok Eio.Path.(t.root / "runs" / run_id)
  else Error (Invalid_id run_id)

let ensure_run_dir t ~run_id =
  Result.map
    (fun d ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 d;
      d)
    (run_dir t ~run_id)

let cassette_path t ~run_id =
  Result.map (fun d -> Eio.Path.(d / "cassette.jsonl")) (run_dir t ~run_id)

type writer = {
  file : Eio.File.rw_ty Eio.Resource.t;
  lock : Eio.Mutex.t;
  mutable closed : bool;
}

let write_line w line = Eio.Flow.copy_string (line ^ "\n") w.file

let open_cassette ~sw t (header : Cassette.header) =
  let ( let* ) = Result.bind in
  let* _ = ensure_run_dir t ~run_id:header.run_id in
  let* path = cassette_path t ~run_id:header.run_id in
  let file =
    Eio.Path.open_out ~sw ~append:true ~create:(`Exclusive 0o600) path
  in
  let w = { file; lock = Eio.Mutex.create (); closed = false } in
  write_line w (Cassette.header_line header);
  Ok w

let append w exchange =
  Eio.Mutex.use_rw ~protect:true w.lock @@ fun () ->
  if w.closed then invalid_arg "Store.append: cassette writer is closed"
  else write_line w (Cassette.exchange_line exchange)

let close w =
  Eio.Mutex.use_rw ~protect:true w.lock @@ fun () ->
  if not w.closed then begin
    w.closed <- true;
    write_line w Cassette.end_line;
    Eio.Resource.close w.file
  end

let exists path =
  match Eio.Path.kind ~follow:true path with
  | `Not_found -> false
  | `Regular_file | `Directory | `Symbolic_link | `Fifo | `Character_special
  | `Block_device | `Socket | `Unknown ->
      true

let read_cassette t ~run_id =
  let ( let* ) = Result.bind in
  let* path = cassette_path t ~run_id in
  if not (exists path) then Error (No_such_cassette run_id)
  else
    Eio.Path.load path |> String.split_on_char '\n' |> Cassette.of_lines
    |> Result.map_error (fun e -> Bad_cassette e)

let inline_limit = 65536
let hash data = Digest.BLAKE256.(to_hex (string data))

let is_digest s =
  String.length s = 64
  && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) s

let blob_path t digest = Eio.Path.(t.root / "blobs" / "blake2b-256" / digest)

let put_blob t data =
  let digest = hash data in
  let path = blob_path t digest in
  if not (exists path) then write_atomic path data;
  digest

let get_blob t digest =
  if not (is_digest digest) then Error (Invalid_digest digest)
  else
    let path = blob_path t digest in
    if not (exists path) then Error (No_such_blob digest)
    else
      let data = Eio.Path.load path in
      if String.equal (hash data) digest then Ok data
      else Error (Blob_corrupt digest)

let payload t data =
  if String.length data <= inline_limit then Cassette.Inline data
  else Cassette.Blob (put_blob t data)

let read_payload t = function
  | Cassette.Inline s -> Ok s
  | Cassette.Blob digest -> get_blob t digest
