# 0005: The replay key covers the query string and behaviour headers

Date: 2026-10-13

## Context

Two requests with identical bodies can still get different responses. Gemini's `streamGenerateContent` answers with Server-Sent Events separated by CRLF when the query has `alt=sse`, and with a pretty-printed JSON array streamed in arbitrary pieces when it does not. Anthropic's `anthropic-beta` and OpenAI's `openai-beta` headers switch features on and off.

## Decision

`Matcher.key` hashes the method, the path, the normalised query string (sorted, the `key` parameter removed) and the headers `anthropic-version`, `anthropic-beta` and `openai-beta`, together with the adapter's canonical request body. Adapters canonicalise bodies only.

## Consequences

A recording made with one framing or beta set never answers a request made with another. Header names outside that list never affect matching.
