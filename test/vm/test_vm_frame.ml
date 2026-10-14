(* The relay's frames: encoding, the incremental decoder, and what it refuses. *)

open Bactrian_core
module F = Vm_frame

let kind_name : F.kind -> string = function
  | Data -> "data"
  | Eof -> "eof"
  | Open -> "open"
  | Close -> "close"
  | Exit -> "exit"
  | Log -> "log"

let show (f : F.t) =
  Printf.sprintf "%d %s %S" f.channel (kind_name f.kind) f.payload

let shown = List.map show

let decode_all bytes =
  let d = F.decoder () in
  match F.feed d bytes with
  | Error e -> Error e
  | Ok fs -> Result.map (fun () -> fs) (F.finish d)

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "decode: %s" (F.describe_error e)

let error = function
  | Ok _ -> Alcotest.fail "expected an error"
  | Error e -> F.describe_error e

let test_wire () =
  Alcotest.(check string)
    "header then payload" "\000\000\000\003\000\000\000\000\002hi"
    (F.encode (List.hd (F.data ~channel:3 "hi")));
  Alcotest.(check string)
    "open carries the port" "\000\000\000\001\002\000\000\000\002\016\146"
    (F.encode (F.open_ ~channel:1 ~port:4242));
  Alcotest.(check (option int))
    "exit status" (Some 143)
    (F.status (List.hd (ok (decode_all (F.encode (F.exit 143))))));
  Alcotest.(check (option int))
    "port" (Some 4242)
    (F.port
       (List.hd (ok (decode_all (F.encode (F.open_ ~channel:1 ~port:4242))))))

let test_data_split () =
  Alcotest.(check int)
    "empty data is no frame" 0
    (List.length (F.data ~channel:0 ""));
  let big = String.make ((2 * F.max_payload) + 1) 'x' in
  Alcotest.(check (list int))
    "split at the limit"
    [ F.max_payload; F.max_payload; 1 ]
    (List.map
       (fun (f : F.t) -> String.length f.payload)
       (F.data ~channel:0 big))

let test_refusals () =
  let header channel kind length =
    let b = Bytes.create 9 in
    Bytes.set_int32_be b 0 (Int32.of_int channel);
    Bytes.set_uint8 b 4 kind;
    Bytes.set_int32_be b 5 (Int32.of_int length);
    Bytes.to_string b
  in
  Alcotest.(check string)
    "unknown kind" "unknown frame kind 9"
    (error (decode_all (header 0 9 0)));
  Alcotest.(check string)
    "too long" "a frame of 65537 bytes, more than the 65536 allowed"
    (error (decode_all (header 0 0 (F.max_payload + 1))));
  Alcotest.(check string)
    "truncated" "input ended 11 bytes into a frame"
    (error (decode_all (header 1 0 5 ^ "ab")));
  (* A spent decoder keeps its error. *)
  let d = F.decoder () in
  ignore (F.feed d (header 0 9 0));
  Alcotest.(check string)
    "spent" "unknown frame kind 9"
    (error (F.feed d (F.encode (F.eof ~channel:0))));
  Alcotest.(check (option int))
    "a short open has no port" None
    (F.port (List.hd (ok (decode_all (header 1 2 1 ^ "x")))))

(* Properties *)

let gen_frame =
  QCheck.Gen.(
    let channel = int_range 0 0x7fff_ffff in
    oneof
      [
        map2
          (fun channel s ->
            match F.data ~channel s with
            | [ f ] -> f
            | [] | _ :: _ :: _ -> F.eof ~channel)
          channel
          (string_size ~gen:char (int_range 1 300));
        map (fun channel -> F.eof ~channel) channel;
        map (fun channel -> F.close ~channel) channel;
        map2
          (fun channel port -> F.open_ ~channel ~port)
          channel (int_range 1 65535);
        map F.exit (int_range 0 255);
        map F.log (string_size ~gen:printable (int_range 0 100));
      ])

let arb_frames =
  QCheck.make
    ~print:(fun fs -> String.concat "\n" (List.map show fs))
    QCheck.Gen.(list_size (int_range 0 20) gen_frame)

let roundtrip =
  QCheck.Test.make ~name:"encode then decode is the identity" ~count:300
    arb_frames (fun fs ->
      let bytes = String.concat "" (List.map F.encode fs) in
      match decode_all bytes with
      | Ok back -> List.equal String.equal (shown fs) (shown back)
      | Error _ -> false)

let chunked =
  QCheck.Test.make ~name:"any split of the bytes decodes the same" ~count:300
    QCheck.(pair arb_frames (list_small nat_small))
    (fun (fs, cuts) ->
      let bytes = String.concat "" (List.map F.encode fs) in
      let n = String.length bytes in
      let points =
        List.sort_uniq Int.compare
          (List.filter_map
             (fun c -> if n = 0 then None else Some (c mod n))
             cuts)
      in
      let rec pieces from = function
        | [] -> [ String.sub bytes from (n - from) ]
        | p :: rest -> String.sub bytes from (p - from) :: pieces p rest
      in
      let d = F.decoder () in
      let back =
        List.concat_map
          (fun piece -> match F.feed d piece with Ok fs -> fs | Error _ -> [])
          (pieces 0 points)
      in
      Result.is_ok (F.finish d)
      && List.equal String.equal (shown fs) (shown back))

let truncated =
  QCheck.Test.make ~name:"a cut inside a frame is reported" ~count:300
    QCheck.(pair arb_frames nat_small)
    (fun (fs, cut) ->
      let bytes = String.concat "" (List.map F.encode fs) in
      let n = String.length bytes in
      QCheck.assume (n > 0);
      let at = cut mod n in
      let boundaries =
        List.fold_left
          (fun acc f -> (List.hd acc + String.length (F.encode f)) :: acc)
          [ 0 ] fs
      in
      match decode_all (String.sub bytes 0 at) with
      | Ok _ -> List.mem at boundaries
      | Error (Truncated _) -> not (List.mem at boundaries)
      | Error (Unknown_kind _ | Too_long _) -> false)

let () =
  Alcotest.run "vm_frame"
    [
      ( "frames",
        [
          Alcotest.test_case "wire format" `Quick test_wire;
          Alcotest.test_case "data is split" `Quick test_data_split;
          Alcotest.test_case "refusals" `Quick test_refusals;
        ] );
      ( "properties",
        List.map
          (fun t -> QCheck_alcotest.to_alcotest t)
          [ roundtrip; chunked; truncated ] );
    ]
