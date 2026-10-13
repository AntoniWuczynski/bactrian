let signals = [ Sys.sigint; Sys.sigterm ]
let exit_status = 130

let with_handlers handlers f =
  let previous =
    List.map (fun (s, h) -> (s, Sys.signal s (Sys.Signal_handle h))) handlers
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun (s, old) -> Sys.set_signal s old) previous)
    f

type trigger = { mutable received : int option; arrived : Eio.Condition.t }

let trigger () = { received = None; arrived = Eio.Condition.create () }

let fire t signal =
  (* Also a signal handler: only a plain write and Condition.broadcast are
     safe. *)
  if Option.is_none t.received then t.received <- Some signal;
  Eio.Condition.broadcast t.arrived

let fired t = t.received

let until t f =
  let rec wait () =
    match t.received with
    | Some signal -> signal
    | None ->
        Eio.Condition.await_no_mutex t.arrived;
        wait ()
  in
  Eio.Fiber.first (fun () -> Error (wait ())) (fun () -> Ok (f ()))

let run ?stop f =
  match stop with
  | Some t -> until t f
  | None ->
      let t = trigger () in
      with_handlers (List.map (fun s -> (s, fire t)) signals) @@ fun () ->
      until t f
