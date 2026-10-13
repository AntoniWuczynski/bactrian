let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec from i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || from (i + 1))
  in
  from 0

let check (collected : Bactrian_core.Sandbox_log.violation list) expected =
  List.iter
    (fun (v : Bactrian_core.Sandbox_log.violation) ->
      Printf.printf "violation: %s %s\n" v.kind v.detail)
    collected;
  let seen (_, kind, needle) =
    List.exists
      (fun (v : Bactrian_core.Sandbox_log.violation) ->
        String.equal v.kind kind && contains v.detail needle)
      collected
  in
  let missing = List.filter (fun e -> not (seen e)) expected in
  List.iter
    (fun (name, _, _) -> Printf.printf "not in the log: %s\n" name)
    missing;
  Alcotest.(check bool)
    "some expected denials collected" true
    (List.length missing < List.length expected)
