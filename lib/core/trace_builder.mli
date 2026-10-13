(** Builds a run's trace from its cassette and what the agent said. *)

type agent_event =
  | Sent_turn of { t_ms : int; turn : int; content : string }
  | Got_reply of { t_ms : int; turn : int; content : string }
  | Got_event of { t_ms : int; turn : int; name : string; data : Canon.t }
  | Got_error of { t_ms : int; turn : int; message : string }
      (** Becomes an [Agent_event] named ["error"] with the message in its
          ["message"] field, because the trace has no error event. *)

type input = {
  run_id : string;
  scenario : string;
  variant : string;
  sim_now : string;
  clock : Trace.clock;
  cassette : Cassette.t;
  agent : agent_event list;  (** In the order they happened. *)
  resolve : Cassette.payload -> string option;  (** Reads blobs. *)
}

val build : input -> Trace.entry list
(** Entries in time order with seq from 0: Run_start, then per turn Turn_start,
    the model calls made during it (each followed by the tool calls its response
    made), tool results the first time a later request carries them (just before
    that request's model call), agent events, the reply, and finally Done. A
    model exchange belongs to the turn in progress at its t_ms. Exchanges whose
    body cannot be parsed by their adapter become a Model_call with no usage and
    a status as recorded (0 for a transport error).

    Model-channel exchanges make the model calls and tool calls. MCP-channel
    exchanges are used only for [tools/call] requests, which are joined to the
    model's tool calls of the same turn (the turn in progress at the exchange's
    [t_ms]) by tool name and equal arguments (spec §6). The k-th call with a
    given name and arguments takes the k-th MCP record with them: [Joined] when
    each side has exactly one, [Joined_by_order] when there are several, and a
    call without a record stays [Unjoined]. An MCP call with no model call to
    join appears on its own, as an [Agent_event] named ["mcp_call"] with the
    fields ["server"], ["tool"] and ["arguments"], placed by time. It is not a
    tool call, so assertions, which count the model's, never count it twice.

    Tool call indexes count from 0 over the whole run and are the indexes
    [Protocol.tool_result.call_index] reports. [t_ms] of a Model_call is the
    request time and never decreases along the trace. *)
