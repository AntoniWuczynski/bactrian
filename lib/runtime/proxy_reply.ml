open Bactrian_core

(* Responses. Bodies are always written chunk by chunk by hand so that each
   upstream piece reaches the agent as its own flushed HTTP chunk, and so that
   a dropped upstream can drop the agent's connection before the terminal
   chunk. *)

type item = Chunk of string | Finished | Dropped

exception Upstream_dropped

(* A failure of the agent's connection, already noted in its run's errors. *)
exception Attributed of exn

type response = Http.Response.t * (Eio.Buf_read.t -> Eio.Buf_write.t -> unit)

let respond ?on_exn ~status ~headers (next : unit -> item) : response =
  let headers =
    Http.Header.add (Http.Header.of_list headers) "transfer-encoding" "chunked"
  in
  ( Http.Response.make ~status:(Http.Status.of_int status) ~headers (),
    fun _ic oc ->
      Eio.Buf_write.flush oc;
      let attribute ex =
        match on_exn with
        | None -> ex
        | Some note_it ->
            note_it ex;
            Attributed ex
      in
      let rec loop () =
        match next () with
        | Chunk "" -> loop ()
        | Chunk s ->
            Eio.Buf_write.printf oc "%x\r\n" (String.length s);
            Eio.Buf_write.string oc s;
            Eio.Buf_write.string oc "\r\n";
            Eio.Buf_write.flush oc;
            loop ()
        | Finished ->
            Eio.Buf_write.string oc "0\r\n\r\n";
            Eio.Buf_write.flush oc
        | Dropped -> raise Upstream_dropped
      in
      try loop () with
      | (Eio.Cancel.Cancelled _ | Upstream_dropped) as ex -> raise ex
      | ex -> raise (attribute ex) )

let bodiless ((head, _) : response) : response =
  ( Http.Response.make
      ~status:(Http.Response.status head)
      ~headers:
        (Http.Header.remove (Http.Response.headers head) "transfer-encoding")
      (),
    fun _ic oc -> Eio.Buf_write.flush oc )

let of_list pieces =
  let rest = ref pieces in
  fun () ->
    match !rest with
    | [] -> Finished
    | x :: tl ->
        rest := tl;
        Chunk x

let error_response ?(extra = []) ?(headers = []) ~status ~type_ message =
  let error =
    Canon.of_fields
      (("type", Canon.string type_)
      :: ("message", Canon.string message)
      :: extra)
  in
  respond ~status
    ~headers:(("content-type", "application/json") :: headers)
    (of_list [ Canon.to_string (Canon.of_fields [ ("error", error) ]) ])
