(* One of the processes of the ledger's two-process test. Usage: ledger_worker
   FILE CAP USD COUNT OWNER. Waits for a line on standard input, so that both
   processes start together, then makes COUNT reservations of USD against an
   openai cap of CAP, leaving each as a usage gap, and prints how many were
   granted. *)

module Ledger = Bactrian_runtime.Ledger

let () =
  match Sys.argv with
  | [| _; file; cap; usd; count; owner |] -> (
      match
        ( float_of_string_opt cap,
          float_of_string_opt usd,
          int_of_string_opt count )
      with
      | Some cap, Some usd, Some count ->
          Eio_main.run @@ fun env ->
          let ledger =
            Ledger.create
              ~path:Eio.Path.(Eio.Stdenv.fs env / file)
              ~caps:[ ("openai", cap) ]
          in
          ignore (In_channel.input_line stdin : string option);
          let granted = ref 0 in
          for _ = 1 to count do
            match
              Ledger.reserve ledger ~owner ~provider:"openai" ~model:"m" ~usd
            with
            | Ok r ->
                incr granted;
                Ledger.settle ledger r ~actual:None
            | Error (Ledger.Cap_reached _ | Unknown_model _ | Unreadable _) ->
                ()
          done;
          print_int !granted;
          print_newline ()
      | _ -> exit 2)
  | _ -> exit 2
