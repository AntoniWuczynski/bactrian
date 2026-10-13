let all : (module Protocol.S) list =
  [
    (module Openai_chat);
    (module Openai_responses);
    (module Anthropic_messages);
    (module Gemini_generate);
    (module Gemini_interactions);
  ]

let find ~meth ~path =
  List.find_opt (fun (module P : Protocol.S) -> P.matches ~meth ~path) all
