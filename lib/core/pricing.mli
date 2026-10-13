(** Built-in prices (spec §11), USD per million tokens. *)

type rates = {
  input : float;
  cached_input : float;
  cache_write : float;
  output : float;
}

type entry = {
  model : string;
      (** Exact ID, also matched as a prefix of dated IDs such as
          ["gpt-5-nano-2025-08-07"]. *)
  provider : string;
      (** ["openai"], ["anthropic"], ["gemini"], ["openrouter"]. *)
  effective_from : string;
      (** ["YYYY-MM-DD"]. The latest entry not after the date applies. *)
  rates : rates;
  long_context : (int * rates) option;
      (** Prompt-token threshold and the rates above it. *)
  max_output : int;
}

val table : entry list

val find : model:string -> date:string -> entry option
(** Exact match first, then the longest table model that is a prefix of [model].
*)

val cost : model:string -> date:string -> Trace.usage -> float option
(** [(input - cached - cache_write)] at the input rate, [cached] at the cached
    rate, [cache_write] at the write rate and [output] at the output rate, using
    the long-context rates when [input] exceeds the threshold. *)

val worst_case :
  model:string ->
  date:string ->
  input_bytes:int ->
  max_output:int option ->
  float option
(** Reservation for a request before it is sent: input tokens estimated as
    [input_bytes / 3] (a deliberate overestimate), output at [max_output] or the
    model's maximum, at long-context rates when the estimate exceeds the
    threshold. [None] for an unknown model, which callers must refuse. *)
