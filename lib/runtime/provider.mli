(** The model providers the proxy serves, and how an agent presents its key to
    each. *)

type t =
  | Openai
  | Anthropic
  | Gemini
  | Openrouter
      (** Path segments "openai", "anthropic", "gemini", "openrouter". Default
          upstreams: https://api.openai.com, https://api.anthropic.com,
          https://generativelanguage.googleapis.com, https://openrouter.ai. *)

val default_upstream : t -> Uri.t
(** The default upstreams listed above. *)

val segment : t -> string
(** The path segment. *)

val of_segment : string -> t option
val equal : t -> t -> bool

val sdk_base : t -> string
(** The part of the path that SDK base URLs include: ["/v1"] for OpenAI,
    ["/api/v1"] for OpenRouter, [""] for the others. *)

val key_header : t -> string
(** The header the provider reads its key from, lowercase. *)

val key_value : t -> string -> string
(** The key as that header carries it ([Bearer] for OpenAI and OpenRouter). *)

val presents_key :
  t ->
  accepted:string list ->
  header:string option ->
  query:string option ->
  bool
(** Whether a request presents one of the [accepted] keys. The key may arrive in
    the provider's key header ([header], its value) and, for Gemini, also in the
    [key] query parameter ([query], ignored for the others). Each one present
    must be accepted, and at least one must be present. *)
