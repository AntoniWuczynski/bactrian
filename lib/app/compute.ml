type t = { run : 'a. (check:(unit -> unit) -> 'a) -> 'a }

let inline = { run = (fun f -> f ~check:ignore) }

(* [Domain_manager.run] runs the work in a fibre of the new domain and cancels
   it when the caller is cancelled, but that cancellation is itself a fibre of
   the new domain, which gets no turn while the work computes. Yielding at each
   check gives it one, and the yield raises once it has run. *)
let in_domain dm =
  {
    run =
      (fun f -> Eio.Domain_manager.run dm (fun () -> f ~check:Eio.Fiber.yield));
  }
