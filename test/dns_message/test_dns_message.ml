module D = Bactrian_core.Dns_message

let u16 n = String.init 2 (fun i -> Char.chr ((n lsr (8 * (1 - i))) land 0xff))

let encode_name labels =
  String.concat ""
    (List.map (fun l -> String.make 1 (Char.chr (String.length l)) ^ l) labels)
  ^ "\000"

(* A query as a stub resolver sends it: RD set, one question, class IN. *)
let query ?(flags = 0x01) ?(count = 1) ~id ~qtype labels =
  u16 id
  ^ String.make 1 (Char.chr flags)
  ^ "\000" ^ u16 count ^ u16 0 ^ u16 0 ^ u16 0 ^ encode_name labels ^ u16 qtype
  ^ u16 1

(* The inverse of the name's escapes, for the round trip. *)
let unescape name =
  if String.equal name "." then []
  else
    let labels = ref [] and b = Buffer.create 16 in
    let n = String.length name in
    let rec go i =
      if i >= n then labels := Buffer.contents b :: !labels
      else
        match name.[i] with
        | '.' ->
            labels := Buffer.contents b :: !labels;
            Buffer.clear b;
            go (i + 1)
        | '\\' ->
            Buffer.add_char b
              (Char.chr (int_of_string (String.sub name (i + 1) 3)));
            go (i + 4)
        | c ->
            Buffer.add_char b c;
            go (i + 1)
    in
    go 0;
    List.rev !labels

let question =
  Alcotest.testable
    (fun ppf (q : D.question) ->
      Format.fprintf ppf "{id=%d; name=%S; qtype=%d}" q.id q.name q.qtype)
    ( = )

let error =
  Alcotest.testable
    (fun ppf e -> Format.pp_print_string ppf (D.describe_error e))
    ( = )

let parses msg expected () =
  Alcotest.(check (result question error)) "parse" expected (D.parse msg)

let example = query ~id:0xbeef ~qtype:28 [ "example"; "com" ]

let units =
  [
    ( "a query",
      parses example (Ok { id = 0xbeef; name = "example.com"; qtype = 28 }) );
    ( "the root",
      parses (query ~id:1 ~qtype:2 []) (Ok { id = 1; name = "."; qtype = 2 }) );
    ( "odd bytes are escaped",
      parses
        (query ~id:7 ~qtype:1 [ "a.b"; "\027[31m"; "x y\\" ])
        (Ok { id = 7; name = "a\\046b.\\027\\09131m.x\\032y\\092"; qtype = 1 })
    );
    ("a short header", parses "\001\002\003" (Error Truncated));
    ( "a truncated name",
      parses
        (String.sub example 0 (String.length example - 8))
        (Error Truncated) );
    ( "a truncated type",
      parses
        (String.sub example 0 (String.length example - 3))
        (Error Truncated) );
    ( "a response",
      parses (query ~flags:0x81 ~id:1 ~qtype:1 [ "a" ]) (Error Not_a_query) );
    ( "two questions",
      parses (query ~count:2 ~id:1 ~qtype:1 [ "a" ]) (Error (Question_count 2))
    );
    ( "a pointer",
      parses
        (u16 1 ^ "\001\000" ^ u16 1 ^ u16 0 ^ u16 0 ^ u16 0 ^ "\192\012" ^ u16 1
       ^ u16 1)
        (Error Compressed_name) );
    ( "a reserved label byte",
      parses
        (u16 1 ^ "\001\000" ^ u16 1 ^ u16 0 ^ u16 0 ^ u16 0 ^ "\064" ^ u16 1
       ^ u16 1)
        (Error (Bad_label 0x40)) );
    ( "a name over 255 bytes",
      parses
        (query ~id:1 ~qtype:1 (List.init 5 (fun _ -> String.make 63 'a')))
        (Error Name_too_long) );
    ( "type names",
      fun () ->
        Alcotest.(check (list string))
          "names"
          [ "A"; "AAAA"; "HTTPS"; "TYPE999" ]
          (List.map D.type_name [ 1; 28; 65; 999 ]) );
    ( "refused",
      fun () ->
        match D.refused example with
        | None -> Alcotest.fail "no reply"
        | Some r ->
            Alcotest.(check string) "id" "\xbe\xef" (String.sub r 0 2);
            Alcotest.(check int) "flags" 0x81 (Char.code r.[2]);
            Alcotest.(check int) "code" 5 (Char.code r.[3]);
            Alcotest.(check string)
              "counts"
              (u16 1 ^ u16 0 ^ u16 0 ^ u16 0)
              (String.sub r 4 8);
            Alcotest.(check string)
              "question"
              (String.sub example 12 (String.length example - 12))
              (String.sub r 12 (String.length r - 12)) );
    ( "refused without a question",
      fun () ->
        Alcotest.(check (option string))
          "header only"
          (Some (u16 1 ^ "\129\005" ^ u16 0 ^ u16 0 ^ u16 0 ^ u16 0))
          (D.refused (query ~count:2 ~id:1 ~qtype:1 [ "a" ])) );
    ( "no reply to a response or a short message",
      fun () ->
        Alcotest.(check (list (option string)))
          "none" [ None; None ]
          [
            D.refused (query ~flags:0x80 ~id:1 ~qtype:1 [ "a" ]);
            D.refused "\000";
          ] );
  ]

let gen_label = QCheck.Gen.(string_size ~gen:char (int_range 1 63))

let gen_labels =
  QCheck.Gen.(
    list_size (int_range 0 4) gen_label
    |> map (fun ls ->
        (* At most 255 bytes on the wire. *)
        let rec fit acc used = function
          | [] -> List.rev acc
          | l :: rest ->
              let n = String.length l + 1 in
              if used + n + 1 > 255 then List.rev acc
              else fit (l :: acc) (used + n) rest
        in
        fit [] 0 ls))

let arb_query =
  QCheck.make
    ~print:(fun (id, qtype, ls) ->
      Printf.sprintf "id=%d qtype=%d %s" id qtype
        (String.concat "|" (List.map String.escaped ls)))
    QCheck.Gen.(triple (int_range 0 65535) (int_range 0 65535) gen_labels)

let round_trip =
  QCheck.Test.make ~count:1000 ~name:"a query's question reads back" arb_query
    (fun (id, qtype, labels) ->
      match D.parse (query ~id ~qtype labels) with
      | Ok q -> q.id = id && q.qtype = qtype && unescape q.name = labels
      | Error _ -> false)

let safe_names =
  QCheck.Test.make ~count:1000 ~name:"names hold only safe characters" arb_query
    (fun (id, qtype, labels) ->
      match D.parse (query ~id ~qtype labels) with
      | Ok q ->
          String.for_all
            (function
              | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' | '\\' ->
                  true
              | _ -> false)
            q.name
      | Error _ -> false)

let total =
  QCheck.Test.make ~count:5000
    ~name:"any bytes give a result, never an exception"
    QCheck.(string_size_of Gen.(int_range 0 80) Gen.char)
    (fun s ->
      ignore (D.parse s : (D.question, D.error) result);
      ignore (D.refused s : string option);
      true)

let refused_echoes =
  QCheck.Test.make ~count:1000 ~name:"refused keeps the ID and the question"
    arb_query (fun (id, qtype, labels) ->
      let q = query ~id ~qtype labels in
      match D.refused q with
      | Some r ->
          String.equal (String.sub r 0 2) (String.sub q 0 2)
          && Char.code r.[3] = 5
          && String.equal
               (String.sub r 12 (String.length r - 12))
               (String.sub q 12 (String.length q - 12))
      | None -> false)

let () =
  Alcotest.run "dns_message"
    [
      ("units", List.map (fun (n, f) -> Alcotest.test_case n `Quick f) units);
      ( "properties",
        List.map
          (fun t -> QCheck_alcotest.to_alcotest t)
          [ round_trip; safe_names; total; refused_echoes ] );
    ]
