(* The agent of a command-mode transcript: reads the lines bactrian writes,
   checks each one against the transcript's next request byte for byte, and
   answers with the transcript's replies. Exits 0 when the input ends exactly
   where the transcript does, and 1 on the first difference. *)

let fail fmt =
  Printf.ksprintf
    (fun m ->
      prerr_endline m;
      exit 1)
    fmt

let rec serve = function
  | [] -> (
      match In_channel.input_line stdin with
      | None -> exit 0
      | Some extra -> fail "unexpected line after the transcript: %s" extra)
  | (e : Transcript.exchange) :: rest -> (
      match In_channel.input_line stdin with
      | None -> fail "input ended, expected: %s" e.request
      | Some line when String.equal line e.request ->
          List.iter
            (fun reply ->
              print_endline reply;
              flush stdout)
            e.replies;
          serve rest
      | Some line -> fail "expected: %s\ngot: %s" e.request line)

let () =
  match Sys.argv with
  | [| _; path |] -> (
      match Transcript.load path with
      | Ok exchanges -> serve exchanges
      | Error m -> fail "%s: %s" path m)
  | _ -> fail "usage: transcript_agent TRANSCRIPT"
