open Bactrian_core

let root = "../../fixtures/protocols"

let fixtures () =
  Sys.readdir root |> Array.to_list
  |> List.concat_map (fun proto ->
      Sys.readdir (Filename.concat root proto)
      |> Array.to_list
      |> List.map (fun case ->
          (proto, Filename.concat (Filename.concat root proto) case)))

let meta dir =
  let text =
    In_channel.with_open_text
      (Filename.concat dir "meta.json")
      In_channel.input_all
  in
  let v = Result.get_ok (Canon.of_string text) in
  ( Option.get (Canon.str_member "method" v),
    Option.get (Canon.str_member "path" v) )

let test_each_fixture_has_one_adapter () =
  let cases = fixtures () in
  Alcotest.(check bool) "fixtures found" true (List.length cases > 10);
  List.iter
    (fun (proto, dir) ->
      let meth, path = meta dir in
      let accepting =
        List.filter
          (fun (module P : Protocol.S) -> P.matches ~meth ~path)
          Protocols.all
        |> List.map (fun (module P : Protocol.S) -> P.name)
      in
      Alcotest.(check (list string))
        (dir ^ " is claimed by its own adapter only")
        [ proto ] accepting;
      match Protocols.find ~meth ~path with
      | Some (module P) -> Alcotest.(check string) (dir ^ " find") proto P.name
      | None -> Alcotest.fail (dir ^ ": no adapter"))
    cases

let test_unknown_path () =
  Alcotest.(check bool)
    "unknown path" true
    (Option.is_none (Protocols.find ~meth:"POST" ~path:"/v1/embeddings"))

let () =
  Alcotest.run "protocols"
    [
      ( "registry",
        [
          Alcotest.test_case "each fixture has one adapter" `Quick
            test_each_fixture_has_one_adapter;
          Alcotest.test_case "unknown path" `Quick test_unknown_path;
        ] );
    ]
