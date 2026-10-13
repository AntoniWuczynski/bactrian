(** What a request can cost beyond what its body shows (spec §5, spend caps).
    Reads the request JSON for the fields that make the provider bill more than
    the body suggests, and prices the worst case. *)

type t = {
  external_input : bool;
      (** The request refers to content the body does not contain:
          [previous_response_id], [previous_interaction_id], [conversation],
          Gemini [cachedContent], or any image, audio, video, document or file
          part (by URL, file id or inline data). *)
  candidates : int;  (** [n] or [candidateCount], at least 1. *)
  fallbacks : string list;
      (** The [models] list of an OpenRouter request: the router may serve any
          of them instead of [model]. *)
}

val analyse : Canon.t -> t

val worst_case :
  model:string ->
  date:string ->
  input_bytes:int ->
  max_output:int option ->
  Canon.t ->
  float option
(** {!Pricing.worst_case} for the request body, at the most expensive of [model]
    and its fallbacks. With external input the prompt is assumed to fill the
    model's context window, and output is reserved once per candidate. [None]
    when any of those models is unknown, so a [:free] model with an unpriced
    fallback is refused, and one with a paid fallback is reserved at the paid
    price. *)

val cost : model:string -> date:string -> Canon.t -> Trace.usage -> float option
(** {!Pricing.cost} at the most expensive of [model] and the request's
    fallbacks, because the response does not say which one served it. [None]
    when any of them is unknown. *)
