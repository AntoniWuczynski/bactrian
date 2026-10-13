open Bactrian_core
module Interrupt = Bactrian_runtime.Interrupt

type ended = Input_closed | Stopped of int

let max_line = 16 * 1024 * 1024

let key : Mcp_msg.id -> string = function
  | Int_id n -> "i" ^ string_of_int n
  | String_id s -> "s" ^ s

let serve ~server ~call ~stop ~log ~input ~output =
  let dispatch = Mcp_dispatch.create server in
  let lock = Eio.Mutex.create () in
  let send message =
    Eio.Mutex.use_rw ~protect:true lock (fun () ->
        try Eio.Flow.copy_string (Mcp_msg.encode message ^ "\n") output
        with Eio.Io _ -> (* the client stopped reading *) ())
  in
  (* The requests in flight, each with the resolver that cancels it. *)
  let in_flight = Hashtbl.create 16 in
  let cancel k =
    Option.iter
      (fun u ->
        Hashtbl.remove in_flight k;
        ignore (Eio.Promise.try_resolve u () : bool))
      (Hashtbl.find_opt in_flight k)
  in
  Eio.Switch.run @@ fun sw ->
  let start id name arguments =
    let k = key id in
    if Hashtbl.mem in_flight k then
      send
        (Error_response
           {
             id = Some id;
             code = -32600;
             message = "Invalid Request: this id is already in flight";
             data = None;
           })
    else
      let cancelled, u = Eio.Promise.create () in
      Hashtbl.replace in_flight k u;
      Eio.Fiber.fork ~sw (fun () ->
          let result =
            Eio.Fiber.first
              (fun () ->
                Eio.Promise.await cancelled;
                None)
              (fun () -> Some (call name arguments))
          in
          (* A cancelled call has left the table already, and its id may be in
             use again. *)
          if not (Eio.Promise.is_resolved cancelled) then
            Hashtbl.remove in_flight k;
          Option.iter
            (fun r -> send (Mcp_dispatch.tool_reply dispatch id r))
            result)
  in
  let reader = Eio.Buf_read.of_flow ~max_size:max_line input in
  let rec loop () =
    match Eio.Buf_read.line reader with
    | exception End_of_file -> ()
    | exception Eio.Buf_read.Buffer_limit_exceeded ->
        log
          (Printf.sprintf "a message is longer than %d bytes: closing" max_line)
    | line when String.equal (String.trim line) "" -> loop ()
    | line ->
        (match Mcp_msg.decode line with
        | Error e -> send (Mcp_dispatch.undecodable e)
        | Ok message -> (
            match Mcp_dispatch.receive dispatch message with
            | Reply m -> send m
            | Call { id; name; arguments } -> start id name arguments
            | Cancel id -> cancel (key id)
            | Ignore -> ()));
        loop ()
  in
  let ended =
    match Interrupt.run ~stop loop with
    | Ok () -> Input_closed
    | Error signal -> Stopped signal
  in
  List.iter cancel (Hashtbl.fold (fun k _ acc -> k :: acc) in_flight []);
  ended

type config = {
  policy : Policy.t;
  bactrian_exe : string;
  on_progress : Bactrian_app.Progress.t -> unit;
  log : string -> unit;
}

let server policy : Mcp_dispatch.server =
  {
    name = "bactrian";
    version = Version.current;
    instructions =
      "bactrian records, replays and statistically compares AI agent runs in \
       this directory, whose .bactrian folder holds the runs and experiments. \
       run_start, record_start and replay_start start a job and return its \
       job_id at once: poll job_status until it is no longer running, and stop \
       it with job_cancel. The other tools answer at once. Every result is the \
       JSON that the matching bactrian command prints with --json. Paths must \
       lie inside this directory. " ^ Policy.describe policy;
    tools = Mcp_tools.definitions;
  }

let run ~env ~root ~stop config ~input ~output =
  Mirage_crypto_rng_unix.use_default ();
  Eio.Switch.run @@ fun sw ->
  let jobs = Jobs.create ~sw in
  let context : Mcp_tools.context =
    {
      env;
      root;
      policy = config.policy;
      jobs;
      bactrian_exe = config.bactrian_exe;
      on_progress = config.on_progress;
    }
  in
  let ended =
    serve ~server:(server config.policy) ~call:(Mcp_tools.call context) ~stop
      ~log:config.log ~input ~output
  in
  Jobs.cancel_all jobs;
  ended
