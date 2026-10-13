(** A run's replay divergences as stored in [divergences.json] and shown in
    reports (design §7). The exchanges themselves stay in the recording, so a
    stored divergence keeps only what a reader needs: its kind, the sequence
    number of the recorded exchange it concerns, the similarity of the nearest
    candidate and the structural diff paths. *)

open Bactrian_core

type kind = Missing_request | Unconsumed | Reordered | Shape_mismatch

type t = {
  kind : kind;
  seq : int option;
      (** The nearest recorded exchange of a missing request (none when nothing
          was left to compare), the unconsumed or expected one otherwise. *)
  similarity : float option;
      (** Of the nearest candidate, missing requests only. *)
  changes : Diff.change list;
      (** From the request to the nearest candidate, missing requests only. *)
}

val of_matcher : Matcher.divergence -> t

val describe : t -> string
(** One line, e.g.
    ["missing request (nearest recorded #3, similarity 0.80, differs at
     messages[0].content)"]. *)

val kind_name : kind -> string
(** ["missing_request"], ["unconsumed"], ["reordered"] or ["shape_mismatch"]. *)

val to_json : t -> Yojson.Safe.t
(** [{"kind", "seq", "similarity", "changes": [{"path", "change"}]}], where
    [change] is ["changed"], ["added"] or ["removed"]. *)

val file_json : t list -> Yojson.Safe.t
(** The content of [divergences.json]: [{"version": 1, "divergences": [...]}].
*)

val of_file_json : Yojson.Safe.t -> (t list, string) result
(** Strict inverse of {!file_json}. *)
