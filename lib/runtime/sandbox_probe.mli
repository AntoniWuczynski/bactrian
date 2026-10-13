(** The sandbox preflight probe (design §5.3): what [bactrian __sandbox-probe]
    does inside a freshly made sandbox, and how bactrian judges its report.

    The probe tries the three things the sandbox must decide: an outbound TCP
    connection to a public address (it must fail, and no data is ever sent), a
    connection to a listener of bactrian's (it must succeed), and a write
    outside the run's state directory (it must fail). It also writes inside the
    state directory, which must succeed, or no sandboxed agent could work. *)

type outcome = (unit, string) result
(** [Error] carries the error's name, such as ["EPERM"], or ["timeout"]. *)

type report = {
  public : outcome;  (** Connecting to [1.1.1.1:443]. *)
  listener : outcome;
      (** Connecting to bactrian's probe listener and reading {!greeting}. *)
  outside_write : outcome;  (** Creating a file outside the state directory. *)
  inside_write : outcome;  (** Creating a file inside the state directory. *)
  netns : string option;
      (** The probe's network namespace ([/proc/self/ns/net]), on Linux. *)
}

val greeting : string
(** What the probe listener writes to every connection. *)

val run : port:int -> outside:string -> inside:string -> report
(** Runs the checks. [port] is the probe listener's loopback port, [outside] and
    [inside] are the files to create. Each connection gives up after 5 seconds.
*)

val to_line : report -> string
(** One JSON object, no newline. *)

val of_line : string -> (report, string) result

val judge :
  Bactrian_core.Sandbox_policy.platform ->
  host_netns:string option ->
  report ->
  (unit, string list) result
(** Every check with the wrong result, described. The public connection must
    fail with Seatbelt's [EPERM] (or [EACCES]) on macOS, so a host that is
    merely offline is not taken for a sandbox, and on Linux the probe's network
    namespace must differ from [host_netns], bactrian's own. *)
