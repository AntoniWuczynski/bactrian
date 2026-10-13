let signal pgid s =
  match Unix.kill (-pgid) s with
  | () -> true
  | exception Unix.Unix_error ((Unix.ESRCH | Unix.EPERM), _, _) -> false

let wait_gone ~clock ~timeout pgid =
  let rec poll () =
    if not (signal pgid 0) then ()
    else (
      Eio.Time.sleep clock 0.02;
      poll ())
  in
  match Eio.Time.with_timeout clock timeout (fun () -> Ok (poll ())) with
  | Ok () -> true
  | Error `Timeout -> false
