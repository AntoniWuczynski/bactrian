type t = Openai | Anthropic | Gemini | Openrouter

let all = [ Openai; Anthropic; Gemini; Openrouter ]

let default_upstream p =
  Uri.of_string
    (match p with
    | Openai -> "https://api.openai.com"
    | Anthropic -> "https://api.anthropic.com"
    | Gemini -> "https://generativelanguage.googleapis.com"
    | Openrouter -> "https://openrouter.ai")

let segment = function
  | Openai -> "openai"
  | Anthropic -> "anthropic"
  | Gemini -> "gemini"
  | Openrouter -> "openrouter"

let of_segment s = List.find_opt (fun p -> String.equal (segment p) s) all
let equal a b = String.equal (segment a) (segment b)

let sdk_base = function
  | Openai -> "/v1"
  | Openrouter -> "/api/v1"
  | Anthropic | Gemini -> ""

let key_header = function
  | Openai | Openrouter -> "authorization"
  | Anthropic -> "x-api-key"
  | Gemini -> "x-goog-api-key"

let key_value p key =
  match p with
  | Openai | Openrouter -> "Bearer " ^ key
  | Anthropic | Gemini -> key

let presents_key p ~accepted ~header ~query =
  let in_header =
    Option.map
      (fun presented ->
        List.exists
          (fun key -> String.equal (key_value p key) presented)
          accepted)
      header
  and in_query =
    match p with
    | Gemini ->
        Option.map
          (fun presented -> List.exists (String.equal presented) accepted)
          query
    | Openai | Anthropic | Openrouter -> None
  in
  match Option.to_list in_header @ Option.to_list in_query with
  | _ :: _ as checks -> List.for_all Fun.id checks
  | [] -> false
