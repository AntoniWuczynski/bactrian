type rates = {
  input : float;
  cached_input : float;
  cache_write : float;
  output : float;
}

type entry = {
  model : string;
  provider : string;
  effective_from : string;
  rates : rates;
  long_context : (int * rates) option;
  max_output : int;
  max_input : int;
}

(* Sources: risk-providers research sections 9 to 12 (2026-10-13) and spec §11.
   OpenAI and Gemini figures came through a summarising fetch and need a
   re-check against the pricing pages. *)

let always = "1970-01-01"

(* Rates are positional: input, cached input, cache write, output. *)
let r input cached_input cache_write output =
  { input; cached_input; cache_write; output }

(* ASSUMPTION: the research does not give a documented maximum output for
   OpenAI or Gemini models, so they use 65536. Only used to size worst-case
   reservations, where a wrong value merely over- or under-reserves. *)
let assumed_max_output = 65_536

(* Anthropic 5.x: 128K maximum output (models overview page). *)
let claude_max_output = 128_000

(* Context windows, in prompt tokens. Claude 5.x: 1M (models overview page).
   ASSUMPTIONS: the research documents no window for Gemini (1M used) and only
   the 272K short-context tier for OpenAI (272K used). They size reservations
   for requests that reference content the body does not contain, so a wrong
   value over- or under-reserves. *)
let claude_max_input = 1_000_000
let assumed_gemini_max_input = 1_000_000
let assumed_openai_max_input = 272_000

let entry ?(effective_from = always) ?long_context ~provider ~max_output
    ~max_input model rates =
  {
    model;
    provider;
    effective_from;
    rates;
    long_context;
    max_output;
    max_input;
  }

let openai =
  entry ~provider:"openai" ~max_output:assumed_max_output
    ~max_input:assumed_openai_max_input

let anthropic =
  entry ~provider:"anthropic" ~max_output:claude_max_output
    ~max_input:claude_max_input

let gemini =
  entry ~provider:"gemini" ~max_output:assumed_max_output
    ~max_input:assumed_gemini_max_input

let table =
  [
    (* OpenAI, standard tier, short context (<= 272K). Long-context rates were
       not verified, so none are listed. Cache write is published only for
       gpt-6-luna (spec §11, 0.125). Elsewhere it is not published and the
       input rate is used. *)
    openai "gpt-6-luna" (r 0.10 0.01 0.125 0.50);
    openai "gpt-5-nano" (r 0.05 0.005 0.05 0.40);
    openai "gpt-6-sol" (r 2.00 0.20 2.00 10.00);
    openai "gpt-6-astra" (r 10.00 1.00 10.00 50.00);
    (* Anthropic: cache write is the 5 minute rate. *)
    anthropic "claude-haiku-5-5" (r 0.10 0.01 0.125 0.50)
      ~long_context:(100_000, r 0.50 0.05 0.625 2.50);
    anthropic "claude-sonnet-5-5" (r 2.00 0.10 2.50 10.00);
    anthropic "claude-opus-5-5" (r 4.00 0.20 5.00 20.00);
    anthropic "claude-fable-5-1" (r 10.00 0.25 12.50 50.00);
    (* Gemini: cache writes are not billed per token (storage is billed hourly
       instead), so the input rate is used. *)
    gemini "gemini-3.8-flash" (r 0.75 0.075 0.75 3.75);
    gemini "gemini-3.8-flash" ~effective_from:"2027-01-01"
      (r 1.50 0.15 1.50 7.50);
    gemini "gemini-3.5-flash-lite" (r 0.30 0.03 0.30 2.50);
    gemini "gemini-3.1-pro-preview" (r 2.00 0.20 2.00 12.00)
      ~long_context:(200_000, r 4.00 0.40 4.00 18.00);
  ]

let free_suffix = ":free"

(* OpenRouter [:free] models cost nothing whatever their id. ASSUMPTION for
   [max_output] and [max_input]: same as the other non-Anthropic providers. *)
let free_entry model =
  entry ~provider:"openrouter" ~max_output:assumed_max_output
    ~max_input:assumed_gemini_max_input model (r 0. 0. 0. 0.)

let matches ~model (e : entry) =
  let n = String.length e.model in
  String.equal model e.model
  || String.length model > n
     && String.starts_with ~prefix:e.model model
     && Char.equal model.[n] '-'

let find ~model ~date =
  if
    String.ends_with ~suffix:free_suffix model
    && String.length model > String.length free_suffix
  then Some (free_entry model)
  else
    let candidates = List.filter (matches ~model) table in
    let longest =
      List.fold_left
        (fun acc (e : entry) -> max acc (String.length e.model))
        0 candidates
    in
    List.fold_left
      (fun best (e : entry) ->
        if
          Int.equal (String.length e.model) longest
          && String.compare e.effective_from date <= 0
        then
          match best with
          | Some (b : entry)
            when String.compare b.effective_from e.effective_from >= 0 ->
              best
          | Some _ | None -> Some e
        else best)
      None candidates

let applicable_rates (e : entry) ~prompt_tokens =
  match e.long_context with
  | Some (threshold, long) when prompt_tokens > threshold -> long
  | Some _ | None -> e.rates

let per_million tokens rate = float_of_int tokens *. rate /. 1e6

let cost ~model ~date (u : Trace.usage) =
  Option.map
    (fun e ->
      let rates = applicable_rates e ~prompt_tokens:u.input_tokens in
      let cached = Option.value u.cached_input_tokens ~default:0 in
      let written = Option.value u.cache_write_tokens ~default:0 in
      let plain = max 0 (u.input_tokens - cached - written) in
      per_million plain rates.input
      +. per_million cached rates.cached_input
      +. per_million written rates.cache_write
      +. per_million u.output_tokens rates.output)
    (find ~model ~date)

let worst_case ~model ~date ~input_bytes ~external_input ~candidates ~max_output
    =
  Option.map
    (fun e ->
      let estimate = max 0 input_bytes / 3 in
      let tokens =
        if external_input then max estimate e.max_input else estimate
      in
      let rates = applicable_rates e ~prompt_tokens:tokens in
      let output =
        max 0 (Option.value max_output ~default:e.max_output) * max 1 candidates
      in
      per_million tokens rates.input +. per_million output rates.output)
    (find ~model ~date)
