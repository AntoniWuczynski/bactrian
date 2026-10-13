open Bactrian_core
module Store = Bactrian_runtime.Store

let get_canon = function
  | Ok v -> v
  | Error _ -> Alcotest.fail "unexpected Canon error"

let header run_id =
  {
    Cassette.run_id;
    scenario = "refund";
    variant = "baseline";
    bactrian_version = "0.1.0";
    recorded_at = "2026-10-13T10:00:00Z";
    sim_now = "2026-01-02T03:04:05Z";
    clock = Trace.Enforced;
  }

let exchange ?(request = Cassette.Inline "{}") seq =
  {
    Cassette.seq;
    t_ms = 10 * seq;
    channel = Cassette.Model;
    protocol = "openai_chat";
    meth = "POST";
    path = "/v1/chat/completions";
    query = [];
    key = "k" ^ string_of_int seq;
    occurrence = 0;
    canonical = get_canon (Canon.of_string {|{"model":"m"}|});
    request;
    outcome =
      Cassette.Response
        {
          status = 200;
          headers = [ ("content-type", "application/json") ];
          body = Cassette.Whole (Cassette.Inline "ok");
        };
  }

(* A fresh directory under the dune sandbox, removed afterwards. *)
let with_tmp name f =
  Eio_main.run @@ fun env ->
  let dir = Eio.Path.(Eio.Stdenv.cwd env / ("tmp-store-" ^ name)) in
  Eio.Path.rmtree ~missing_ok:true dir;
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir)
    (fun () -> f dir)

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "store error: %a" Store.pp_error e

let show e = Format.asprintf "%a" Store.pp_error e

let expect_error expected = function
  | Error got -> Alcotest.(check string) "error" (show expected) (show got)
  | Ok _ -> Alcotest.failf "expected %s" (show expected)

let test_atomic_write () =
  with_tmp "atomic" @@ fun dir ->
  let target = Eio.Path.(dir / "sub" / "meta.json") in
  Store.write_atomic target "one";
  Alcotest.(check string) "created with parents" "one" (Eio.Path.load target);
  Store.write_atomic target "two";
  Alcotest.(check string) "replaced" "two" (Eio.Path.load target);
  Alcotest.(check (list string))
    "no temporary left" [ "meta.json" ]
    (Eio.Path.read_dir Eio.Path.(dir / "sub"))

let test_atomic_crash () =
  with_tmp "crash" @@ fun dir ->
  let target = Eio.Path.(dir / "meta.json") in
  Store.write_atomic target "old";
  (try
     Store.write_atomic ~before_rename:(fun () -> raise Exit) target "new";
     Alcotest.fail "hook did not run"
   with Exit -> ());
  Alcotest.(check string)
    "target untouched after a crash before rename" "old" (Eio.Path.load target);
  Store.write_atomic target "newer";
  Alcotest.(check string)
    "later write succeeds beside the stale temporary file" "newer"
    (Eio.Path.load target)

let test_atomic_crash_new_file () =
  with_tmp "crash-new" @@ fun dir ->
  let target = Eio.Path.(dir / "meta.json") in
  (try Store.write_atomic ~before_rename:(fun () -> raise Exit) target "x"
   with Exit -> ());
  Alcotest.(check bool)
    "target does not exist" true
    (match Eio.Path.kind ~follow:true target with
    | `Not_found -> true
    | `Regular_file | `Directory | `Symbolic_link | `Fifo | `Character_special
    | `Block_device | `Socket | `Unknown ->
        false)

let test_cassette_every_line () =
  with_tmp "cassette" @@ fun dir ->
  let store = Store.create ~root:dir in
  Eio.Switch.run @@ fun sw ->
  let w = ok (Store.open_cassette ~sw store (header "run-1")) in
  let check n complete =
    let c = ok (Store.read_cassette store ~run_id:"run-1") in
    Alcotest.(check int)
      (Printf.sprintf "%d exchanges" n)
      n (List.length c.exchanges);
    Alcotest.(check bool) "complete" complete c.complete
  in
  check 0 false;
  for seq = 0 to 4 do
    Store.append w (exchange seq);
    check (seq + 1) false
  done;
  Store.close w;
  check 5 true;
  Store.close w;
  check 5 true;
  Alcotest.check_raises "append after close"
    (Invalid_argument "Store.append: cassette writer is closed") (fun () ->
      Store.append w (exchange 5));
  let c = ok (Store.read_cassette store ~run_id:"run-1") in
  Alcotest.(check bool)
    "exchanges round-trip" true
    (List.for_all2 Cassette.equal_exchange c.exchanges
       (List.init 5 (fun i -> exchange i)))

let test_unclosed_is_partial () =
  with_tmp "partial" @@ fun dir ->
  let store = Store.create ~root:dir in
  ( Eio.Switch.run @@ fun sw ->
    let w = ok (Store.open_cassette ~sw store (header "run-2")) in
    Store.append w (exchange 0) );
  let c = ok (Store.read_cassette store ~run_id:"run-2") in
  Alcotest.(check (pair int bool))
    "one exchange, no end line" (1, false)
    (List.length c.exchanges, c.complete)

let test_read_missing () =
  with_tmp "missing" @@ fun dir ->
  let store = Store.create ~root:dir in
  expect_error (Store.No_such_cassette "nope")
    (Store.read_cassette store ~run_id:"nope")

let test_run_ids () =
  with_tmp "ids" @@ fun dir ->
  let store = Store.create ~root:dir in
  List.iter
    (fun id ->
      match Store.run_dir store ~run_id:id with
      | Error e -> expect_error (Store.Invalid_id id) (Error e)
      | Ok _ -> Alcotest.failf "accepted %S" id)
    [ ""; "."; ".."; "a/b"; "../x"; "a\000b" ];
  ignore (ok (Store.run_dir store ~run_id:"2026-10-13-abc"))

let test_blobs () =
  with_tmp "blobs" @@ fun dir ->
  let store = Store.create ~root:dir in
  let data = String.make 100_000 'x' in
  let d = Store.put_blob store data in
  Alcotest.(check string)
    "digest is BLAKE2b-256 of the bytes"
    Digest.BLAKE256.(to_hex (string data))
    d;
  Alcotest.(check string) "same digest again" d (Store.put_blob store data);
  Alcotest.(check string) "read back" data (ok (Store.get_blob store d));
  expect_error
    (Store.No_such_blob (String.make 64 '0'))
    (Store.get_blob store (String.make 64 '0'));
  expect_error (Store.Invalid_digest "../../etc/passwd")
    (Store.get_blob store "../../etc/passwd");
  Eio.Path.save ~create:(`Or_truncate 0o600)
    Eio.Path.(dir / "blobs" / "blake2b-256" / d)
    "tampered";
  expect_error (Store.Blob_corrupt d) (Store.get_blob store d)

let test_payload () =
  with_tmp "payload" @@ fun dir ->
  let store = Store.create ~root:dir in
  let small = String.make Store.inline_limit 'a' in
  (match Store.payload store small with
  | Cassette.Inline s -> Alcotest.(check string) "inline" small s
  | Cassette.Blob _ -> Alcotest.fail "limit-sized payload should be inline");
  let big = small ^ "b" in
  let p = Store.payload store big in
  (match p with
  | Cassette.Blob _ -> ()
  | Cassette.Inline _ -> Alcotest.fail "oversize payload should be a blob");
  Alcotest.(check string) "read_payload" big (ok (Store.read_payload store p))

let () =
  Alcotest.run "store"
    [
      ( "atomic",
        [
          Alcotest.test_case "write and replace" `Quick test_atomic_write;
          Alcotest.test_case "crash before rename" `Quick test_atomic_crash;
          Alcotest.test_case "crash, new file" `Quick test_atomic_crash_new_file;
        ] );
      ( "cassette",
        [
          Alcotest.test_case "decodes after every line" `Quick
            test_cassette_every_line;
          Alcotest.test_case "unclosed is partial" `Quick
            test_unclosed_is_partial;
          Alcotest.test_case "missing" `Quick test_read_missing;
          Alcotest.test_case "run ids" `Quick test_run_ids;
        ] );
      ( "blobs",
        [
          Alcotest.test_case "put, get, verify" `Quick test_blobs;
          Alcotest.test_case "inline limit" `Quick test_payload;
        ] );
    ]
