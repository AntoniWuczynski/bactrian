(** Built-in prices (design §11), USD per million tokens. *)

type rates = {
  input : float;
  cached_input : float;
  cache_write : float;
  output : float;
}

val providers : string list
(** The provider names an entry or a spend cap may use. *)

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
  max_input : int;
      (** Context window in prompt tokens. Some values are assumptions, marked
          in the table. *)
}

val table : entry list
(** The built-in entries. *)

type table
(** A price table: the built-in entries, optionally changed by an experiment's
    price file. Immutable. *)

val default : table
(** The built-in entries only. {!find}, {!cost} and {!worst_case} use it. *)

val with_overrides : entry list -> table
(** The built-in entries with [entries] applied: an entry replaces the built-in
    one of the same [model] and [effective_from], and any other adds a model or
    another period of one. *)

val find_in : table -> model:string -> date:string -> entry option

val cost_in :
  table -> model:string -> date:string -> Trace.usage -> float option

val worst_case_in :
  table ->
  model:string ->
  date:string ->
  input_bytes:int ->
  external_input:bool ->
  candidates:int ->
  max_output:int option ->
  float option
(** {!find}, {!cost} and {!worst_case} in a given table. *)

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
  external_input:bool ->
  candidates:int ->
  max_output:int option ->
  float option
(** Reservation for a request before it is sent: input tokens estimated as
    [input_bytes / 3] (a deliberate overestimate), or at least the model's
    [max_input] when [external_input] says the request refers to server-side or
    external content the body does not contain. Output is [max_output] or the
    model's maximum, once per candidate ([candidates] below 1 count as 1). Long
    context rates apply when the input exceeds the threshold. [None] for an
    unknown model, which callers must refuse. *)
