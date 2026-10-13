(** The arguments of a tool: their JSON Schema 2020-12 (the [inputSchema] of
    [tools/list]) and their strict decoding, both from one hand-written table.
*)

open Bactrian_core

type kind =
  | String
  | Strings  (** An array of strings. *)
  | Nonempty_strings  (** An array of at least one string. *)
  | Number
  | Numbers  (** An array of numbers. *)
  | Integer
  | Boolean
  | Enum of string list  (** One of these strings. *)

type property = {
  name : string;
  kind : kind;
  required : bool;
  description : string;
}

val input_schema : property list -> Canon.t
(** [{"type": "object", "properties": …, "required": […],
     "additionalProperties": false}], with each property's [description]. *)

val decode :
  property list ->
  (Yojson.Safe.t -> ('a, string) result) ->
  Canon.t option ->
  ('a, string) result
(** [decode properties of_yojson arguments] checks that [arguments] (absent
    means [{}]) is an object with no member outside [properties], every required
    one present, each enumeration's value one of its strings and each non-empty
    array not empty, then decodes it with [of_yojson], [ppx_deriving_yojson]
    decoders whose record fields are named as the properties. Since every member
    was checked against [properties] first, a decoder that covers only some of
    them (a part that several tools share) keeps the whole decoding strict.
    Every [Error] names the argument at fault. *)
