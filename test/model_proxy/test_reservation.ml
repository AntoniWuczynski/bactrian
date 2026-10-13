open Bactrian_core
open Proxy_support

(* Reservations at the proxy: a request that would bill far more than its body
   suggests must be refused under a tight cap, while the same request without
   the reference passes. *)

let tight = 0.01
let tight_caps = List.map (fun (p, _) -> (p, tight)) caps

let edit f body =
  let v = canon body in
  Canon.to_string (f v)

let add k x = edit (Canon.set_member k x)

let parse s =
  match Canon.of_string s with
  | Ok v -> v
  | Error e -> Alcotest.failf "bad JSON: %a" Canon.pp_error e

let json_member k s = Canon.set_member k (parse s)

type case = {
  name : string;
  protocol : string;
  fixture : string;
  body_of : string -> string;
  expect : [ `Refused of string | `Allowed ];
}

let run_case c =
  Alcotest.test_case c.name `Quick
  @@ with_ctx ("res-" ^ c.protocol ^ "-" ^ c.fixture)
  @@ fun ctx ->
  let f = fixture c.protocol c.fixture in
  let up = fake ctx (F.Fixtures fixtures_root) in
  let run =
    make_run ~caps:tight_caps ctx ~token:token_a ~upstream:(F.base_url up)
  in
  P.register ctx.proxy run;
  let r = send_fixture ctx ~token:token_a ~body_of:c.body_of f in
  ignore (P.unregister ctx.proxy ~token:token_a);
  match c.expect with
  | `Refused type_ ->
      Alcotest.(check int) "status" 402 r.status;
      Alcotest.(check string) "error type" type_ (error_type r);
      Alcotest.(check int) "nothing forwarded" 0 (List.length (F.requests up))
  | `Allowed ->
      Alcotest.(check bool) "not refused" true (r.status <> 402);
      Alcotest.(check int) "forwarded" 1 (List.length (F.requests up))

let cap = `Refused "bactrian_cap_reached"
let unknown = `Refused "bactrian_unknown_model"
let chat = ("openai_chat", "luna_nonstream")
let responses = ("openai_responses", "luna_nonstream")
let gemini = ("gemini_generate", "flash_generate")
let anthropic = ("anthropic_messages", "haiku_nonstream")
let openrouter = ("openai_chat", "openrouter_nemotron_stream")

let image_message =
  {|[{"role":"user","content":[{"type":"image_url","image_url":{"url":"https://example.com/a.png"}}]}]|}

let cases =
  [
    ("baseline chat", chat, Fun.id, `Allowed);
    ("chat with n multiplies output", chat, add "n" (Canon.int 100), cap);
    ("baseline responses", responses, Fun.id, `Allowed);
    ( "previous_response_id reserves the context window",
      responses,
      add "previous_response_id" (Canon.string "resp_123"),
      cap );
    ( "conversation reserves the context window",
      responses,
      add "conversation" (Canon.string "conv_123"),
      cap );
    ( "image by URL reserves the context window",
      chat,
      edit (json_member "messages" image_message),
      cap );
    ("baseline gemini", gemini, Fun.id, `Allowed);
    ( "cachedContent reserves the context window",
      gemini,
      add "cachedContent" (Canon.string "cachedContents/abc"),
      cap );
    ( "candidateCount multiplies output",
      gemini,
      edit (fun v ->
          Canon.set_member "generationConfig"
            (Canon.set_member "candidateCount" (Canon.int 100)
               (Option.value
                  (Canon.member "generationConfig" v)
                  ~default:Canon.empty_object))
            v),
      cap );
    ("baseline anthropic", anthropic, Fun.id, `Allowed);
    ( "anthropic file document reserves the context window",
      anthropic,
      edit
        (json_member "messages"
           {|[{"role":"user","content":[{"type":"document","source":{"type":"file","file_id":"file_1"}}]}]|}),
      cap );
    ("free openrouter model is free", openrouter, Fun.id, `Allowed);
    ( "free model with a paid fallback is not free",
      openrouter,
      add "models" (Canon.list [ Canon.string "gpt-6-astra" ]),
      cap );
    ( "unknown openrouter fallback is refused",
      openrouter,
      add "models" (Canon.list [ Canon.string "no-such-model" ]),
      unknown );
  ]
  |> List.map (fun (name, (protocol, fixture), body_of, expect) ->
      run_case { name; protocol; fixture; body_of; expect })

let () = Alcotest.run "reservation" [ ("proxy", cases) ]
