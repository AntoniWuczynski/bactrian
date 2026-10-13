(* The weather conversation of test/fixtures/protocols/openai_chat as a server
   process: a request that already holds a tool message gets the second
   recorded reply, any other the first, whatever the model or the system
   prompt. It prints its port as its first line, serves until its standard
   input closes, and holds every request while the --hold file exists. *)

open Cmdliner
module F = Fake_upstream
module Canon = Bactrian_core.Canon

let fixtures =
  Arg.(
    required
    & opt (some string) None
    & info [ "fixtures" ] ~docv:"DIR"
        ~doc:"The openai_chat fixture directory (luna_nonstream, luna_turn2).")

let hold =
  Arg.(
    value
    & opt (some string) None
    & info [ "hold" ] ~docv:"FILE"
        ~doc:"Do not answer a request while $(docv) exists.")

let log =
  Arg.(
    value
    & opt (some string) None
    & info [ "log" ] ~docv:"FILE"
        ~doc:"Append the path of every request to $(docv).")

let has_tool_message (r : F.request) =
  match Canon.of_string r.body with
  | Error _ -> false
  | Ok body ->
      List.exists
        (fun m ->
          Option.equal String.equal (Canon.str_member "role" m) (Some "tool"))
        (Option.value ~default:[] (Canon.list_member "messages" body))

let reply dir case =
  match F.case_reply (Filename.concat dir case) with
  | Ok r -> r
  | Error e ->
      Format.eprintf "fake_upstream_server: %a@." F.pp_error e;
      exit 2

let main fixtures hold log =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let first = reply fixtures "luna_nonstream"
  and second = reply fixtures "luna_turn2" in
  let clock = Eio.Stdenv.clock env in
  let handler (r : F.request) =
    Option.iter
      (fun path ->
        Out_channel.with_open_gen [ Open_append; Open_creat ] 0o600 path
          (fun oc -> output_string oc (r.path ^ "\n")))
      log;
    Option.iter
      (fun path ->
        while Sys.file_exists path do
          Eio.Time.sleep clock 0.02
        done)
      hold;
    if has_tool_message r then second else first
  in
  match F.start ~sw ~net:(Eio.Stdenv.net env) (F.Handler handler) with
  | Error e ->
      Format.eprintf "fake_upstream_server: %a@." F.pp_error e;
      2
  | Ok up ->
      print_endline (string_of_int (F.port up));
      (* The parent holds the other end of standard input. *)
      ignore
        (Eio.Buf_read.take_all
           (Eio.Buf_read.of_flow ~max_size:max_int (Eio.Stdenv.stdin env))
          : string);
      0

let () =
  exit
    (Cmd.eval'
       (Cmd.v
          (Cmd.info "fake_upstream_server"
             ~doc:"A fake OpenAI-compatible upstream.")
          Term.(const main $ fixtures $ hold $ log)))
