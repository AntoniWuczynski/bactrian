(** The normalised record of one run, as JSON Lines. Each entry is one JSON
    object carrying ["v"], ["seq"], ["t_ms"] and ["type"], plus the fields of
    its event. Model-call fields use OpenTelemetry GenAI names such as
    ["gen_ai.request.model"] and ["gen_ai.usage.input_tokens"]. Decoding is
    strict: unknown fields and unknown types are errors. *)

val schema_version : int
(** 1 *)

type clock = Enforced | Contract_only

type usage = {
  input_tokens : int;  (** ["gen_ai.usage.input_tokens"] *)
  output_tokens : int;  (** ["gen_ai.usage.output_tokens"] *)
  cached_input_tokens : int option;  (** ["gen_ai.usage.cached_input_tokens"] *)
  cache_write_tokens : int option;  (** ["gen_ai.usage.cache_write_tokens"] *)
  reasoning_tokens : int option;  (** ["gen_ai.usage.reasoning_tokens"] *)
}

type join =
  | Joined
  | Joined_by_order
  | Unjoined
      (** How an MCP record was matched to a model tool call (spec §6). *)

type event =
  | Run_start of {
      run_id : string;
      scenario : string;
      variant : string;
      sim_now : string;
      clock : clock;
    }
  | Turn_start of { turn : int; content : string }
  | Model_call of {
      turn : int;
      protocol : string;
      model : string;  (** ["gen_ai.request.model"] *)
      status : int;
      usage : usage option;
      duration_ms : int;
      ttft_ms : int option;
    }
  | Tool_call of {
      turn : int;
      index : int;
      name : string;
      arguments : Canon.t;
      mcp : join;
    }
  | Tool_result of {
      turn : int;
      index : int;
      content : string;
      is_error : bool;
    }
  | Http_call of {
      turn : int;
      meth : string;  (** ["method"] on the wire. *)
      host : string;
      path : string;
      status : int;
    }
  | Agent_event of { turn : int; name : string; data : Canon.t }
  | Reply of { turn : int; content : string }
  | Sandbox_violation of { kind : string; detail : string }
  | Done

type entry = { seq : int; t_ms : int; event : event }

type error =
  | Invalid_json of string
  | Unsupported_version of int
  | Unknown_type of string
  | Bad_entry of { type_ : string; reason : string }

val to_line : entry -> string
(** One JSON object, no trailing newline. *)

val of_line : string -> (entry, error) result
val equal_entry : entry -> entry -> bool
val pp_error : Format.formatter -> error -> unit
