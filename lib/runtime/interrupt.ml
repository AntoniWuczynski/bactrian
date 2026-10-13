let signals = [ Sys.sigint; Sys.sigterm ]
let exit_status = 130

let run f =
  let received = ref None in
  let arrived = Eio.Condition.create () in
  let handle signal =
    (* A signal handler: only a plain write and Condition.broadcast are safe. *)
    if Option.is_none !received then received := Some signal;
    Eio.Condition.broadcast arrived
  in
  let previous =
    List.map (fun s -> (s, Sys.signal s (Sys.Signal_handle handle))) signals
  in
  Fun.protect ~finally:(fun () ->
      List.iter (fun (s, old) -> Sys.set_signal s old) previous)
  @@ fun () ->
  let rec wait () =
    match !received with
    | Some signal -> signal
    | None ->
        Eio.Condition.await_no_mutex arrived;
        wait ()
  in
  Eio.Fiber.first (fun () -> Error (wait ())) (fun () -> Ok (f ()))
