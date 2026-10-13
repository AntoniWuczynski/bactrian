let flag_group =
  Re.(
    compile
      (seq
         [
           bos;
           str "(?";
           rep1 (alt [ rg 'a' 'z'; rg 'A' 'Z'; char '-' ]);
           char ')';
         ]))

let compile ?(allow_empty_match = false) source =
  let caseless = String.starts_with ~prefix:"(?i)" source in
  let body =
    if caseless then String.sub source 4 (String.length source - 4) else source
  in
  if Re.execp flag_group body then
    Error "inline flags other than a leading (?i) are not supported"
  else
    match Re.Perl.re_result body with
    | Error `Parse_error -> Error "invalid syntax"
    | Error `Not_supported -> Error "unsupported syntax"
    | Ok core ->
        let re = Re.compile (if caseless then Re.no_case core else core) in
        if (not allow_empty_match) && Re.execp re "" then
          Error "matches the empty string"
        else Ok re
