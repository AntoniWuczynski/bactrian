(** The experiment's price file: entries that override or extend the built-in
    table ({!Pricing.with_overrides}). Strict, like the other formats.

    {v
    version: 1
    prices:
      - model: gpt-6-luna            # exact ID, also a prefix of dated IDs
        provider: openai             # openai | anthropic | gemini | openrouter
        effective_from: 2026-11-01   # optional, default 1970-01-01
        rates: { input: 0.2, cached_input: 0.02, cache_write: 0.25, output: 1 }
        long_context:                # optional
          above: 200000              # prompt tokens
          rates: { input: 0.4, cached_input: 0.04, cache_write: 0.5, output: 2 }
        max_output: 65536
        max_input: 272000
    v}

    Rates are USD per million tokens. *)

val decode : Canon.t -> (Pricing.entry list, Config_error.error list) result
