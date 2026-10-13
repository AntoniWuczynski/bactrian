open Config_decode

let rates ~path c : (Pricing.rates, _) result =
  object_ ~path c (fun r ->
      let rate key = required r key (float_at_least 0.0) in
      let+ input = rate "input"
      and+ cached_input = rate "cached_input"
      and+ cache_write = rate "cache_write"
      and+ output = rate "output" in
      { Pricing.input; cached_input; cache_write; output })

let long_context ~path c =
  object_ ~path c (fun r ->
      let+ above = required r "above" (int_at_least 1)
      and+ rates = required r "rates" rates in
      (above, rates))

let date ~path c =
  let* s = string ~path c in
  let digits_at i n =
    String.for_all
      (function '0' .. '9' -> true | _ -> false)
      (String.sub s i n)
  in
  if
    String.length s = 10
    && digits_at 0 4
    && Char.equal s.[4] '-'
    && digits_at 5 2
    && Char.equal s.[7] '-'
    && digits_at 8 2
  then Ok s
  else
    fail ~path (Printf.sprintf "expected a date such as 2026-11-01, found %S" s)

let provider =
  enum ~what:"provider" (List.map (fun p -> (p, p)) Pricing.providers)

let entry ~path c : (Pricing.entry, _) result =
  object_ ~path c (fun r ->
      let+ model = required r "model" non_empty_string
      and+ provider = required r "provider" provider
      and+ effective_from = default r "effective_from" "1970-01-01" date
      and+ rates = required r "rates" rates
      and+ long_context = optional r "long_context" long_context
      and+ max_output = required r "max_output" (int_at_least 1)
      and+ max_input = required r "max_input" (int_at_least 1) in
      {
        Pricing.model;
        provider;
        effective_from;
        rates;
        long_context;
        max_output;
        max_input;
      })

let decode c =
  object_ ~path:"" c (fun r ->
      let+ _version = required r "version" (version 1)
      and+ prices = required r "prices" (list entry) in
      prices)
