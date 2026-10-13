(** [bactrian replay]: run an agent against a recording, without calling
    providers. *)

type mode =
  | Strict  (** Refuse any request the recording lacks. *)
  | Until_divergence  (** Go live at the first request it lacks. *)
  | Fuzzy  (** Serve the nearest recorded exchange above the threshold. *)

val modes : (string * mode) list
(** The names of the modes: [strict], [until-divergence] and [fuzzy]. *)

val timings : (string * Bactrian_runtime.Model_proxy.timing) list
(** The names of the stream timings: [fast] and [recorded]. *)

type request = {
  common : Single_run.common;
  from : string;  (** Run ID or run directory to replay. *)
  mode : mode;
  threshold : float option;
      (** Minimum similarity for [Fuzzy], from 0 to 1 (0.9 when [None]). *)
  allow_reorder : bool;  (** Serve recorded exchanges out of order. *)
  timing : Bactrian_runtime.Model_proxy.timing;
}

val validate : request -> (unit, string) result
(** [Error] when the threshold is out of range or given for another mode, or a
    timeout is not positive. {!run} checks the same. *)

val run :
  ?stop:Bactrian_runtime.Interrupt.trigger ->
  on_progress:(Progress.t -> unit) ->
  bactrian_exe:string ->
  env:Eio_unix.Stdenv.base ->
  request ->
  (Bactrian_runtime.Run.result, Command_error.t) result
(** Replays the recording (see {!Single_run.execute}, which says what the caller
    provides). Only [Until_divergence] can go live, so only it needs a spend cap
    and key. An invalid request is a [Usage] error. *)

val exit_code : Bactrian_runtime.Run.result -> int
(** {!Single_run.exit_code}. *)

val to_json : Bactrian_runtime.Run.result -> Yojson.Safe.t
(** {!Single_run.to_json}. *)
