open Config_decode

type turn = { user : string }

type t = {
  id : string;
  now : string option;
  timezone : string option;
  fixtures : string option;
  turns : turn list;
  assertions : Assertion.t list;
  checkers : string list list;
  golden_pass : string list;
  golden_fail : string list;
}

type error = Config_error.error = { path : string; message : string }

let turn ~path c =
  object_ ~path c (fun r ->
      let+ user = required r "user" string in
      { user })

(* [Assertion.decode] reports paths relative to the assertion. *)
let assertion ~path c =
  Result.map_error
    (List.map (fun (e : error) -> { e with path = nest path e.path }))
    (Assertion.decode c)

let argv = non_empty_list string
let checker ~path c = object_ ~path c (fun r -> required r "command" argv)

let golden ~path c =
  object_ ~path c (fun r ->
      let+ pass = default r "pass" [] (list string)
      and+ fail = default r "fail" [] (list string) in
      (pass, fail))

let decode c =
  object_ ~path:"" c (fun r ->
      let+ _version = required r "version" (version 1)
      and+ id = required r "id" non_empty_string
      and+ now = optional r "now" string
      and+ timezone = optional r "timezone" string
      and+ fixtures = optional r "fixtures" string
      and+ turns = required r "turns" (non_empty_list turn)
      and+ assertions = default r "assertions" [] (list assertion)
      and+ checkers = default r "checkers" [] (list checker)
      and+ golden_pass, golden_fail = default r "golden" ([], []) golden in
      {
        id;
        now;
        timezone;
        fixtures;
        turns;
        assertions;
        checkers;
        golden_pass;
        golden_fail;
      })
