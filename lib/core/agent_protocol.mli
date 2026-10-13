(** The wire contract between bactrian and an agent (spec §5.1), version 1. One
    JSON object per line. *)

type to_agent =
  | Start of {
      run_id : string;
      scenario : string;
      variant : string;
      sim_now : string;
      state_dir : string;
    }
  | Turn of { turn : int; content : string }
  | End

type from_agent =
  | Reply of { turn : int; content : string }
  | Event of { turn : int; name : string; data : Canon.t }
  | Agent_error of { turn : int; message : string }
      (** ["type":"error"] on the wire. *)

type error =
  | Invalid_json of string
  | Unsupported_version of int
  | Unknown_type of string
  | Bad_message of { type_ : string; reason : string }

val encode : to_agent -> string
val decode : string -> (from_agent, error) result

val encode_from_agent : from_agent -> string
(** For tests and example agents. *)

val decode_to_agent : string -> (to_agent, error) result
val pp_error : Format.formatter -> error -> unit
