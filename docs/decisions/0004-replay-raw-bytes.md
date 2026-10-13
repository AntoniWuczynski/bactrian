# 0004: Replay recorded bytes unchanged

Date: 2026-10-13

## Context

A replayed response could be rebuilt from parsed events or sent back exactly as recorded. Providers send details that no parser models: padding fields that vary per request, keep-alive comment lines, CRLF in one provider's streams and LF in the others, event types added after the parser was written.

## Decision

Replay returns the recorded response bytes with the recorded chunk boundaries, and optionally the recorded timing. Protocol adapters only parse: they canonicalise requests for matching and summarise responses for the trace and cost accounting. They never re-serialise a response.

## Consequences

Replay is as faithful as the recording. A parser bug can produce a wrong summary but cannot corrupt what the agent receives. Test fixtures are stored as raw bytes with their chunk boundaries.
