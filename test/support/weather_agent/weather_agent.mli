(** The Python agent the fixtures in [test/fixtures/protocols/openai_chat] were
    recorded for: one weather question, one [get_weather] call, one reply. It
    writes its environment to [env.json] in the state directory. An optional
    first argument is a system prompt. Run it as [python3 -I -c script]. *)

val script : string
