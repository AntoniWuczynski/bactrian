open Config_decode

type agent =
  | Command of {
      argv : string list;
      turn_timeout : float;
      total_timeout : float;
    }
  | Http of {
      url : string;
      namespace_header : string;
      turn_timeout : float;
      total_timeout : float;
    }

type variant = { name : string; env : (string * string) list }
type policy = Record | Replay | Passthrough | Block

type compare = {
  baseline : string;
  confidence : float;
  thresholds : (string * float) list;
  gate : string list;
  population : [ `Suite | `Scenarios ];
}

type t = {
  name : string;
  agent : agent;
  scenarios : string list;
  variants : variant list;
  repeats : int;
  concurrency : int;
  intercept : [ `Model | `Mcp | `Fixtures | `Egress ] list;
  egress_default : policy;
  egress_hosts : (string * policy) list;
  mcp : (string * policy) list;
  normalise : string list;
  redact : string list;
  sandbox : [ `Auto | `Strict | `Off ];
  faketime : bool;
  caps_usd : (string * float) list;
  pricing : string option;
  seed : int;
  env_passthrough : string list;
  compare : compare option;
}

type error = Scenario.error

let policy =
  enum ~what:"policy"
    [
      ("record", Record);
      ("replay", Replay);
      ("passthrough", Passthrough);
      ("block", Block);
    ]

let timeouts ~path c =
  object_ ~path c (fun r ->
      let+ turn = default r "turn" 120.0 duration
      and+ total = default r "total" 600.0 duration in
      (turn, total))

let forbidden r key ~protocol =
  match optional r key (fun ~path:_ c -> Ok c) with
  | Ok None -> Ok ()
  | Ok (Some _) ->
      fail
        ~path:(field_path (path r) key)
        (Printf.sprintf "not allowed with protocol %s" protocol)
  | Error e -> Error e

let agent ~path c =
  object_ ~path c (fun r ->
      let common =
        let+ timeouts = default r "timeouts" (120.0, 600.0) timeouts
        and+ env = default r "env_passthrough" [] (list string) in
        (timeouts, env)
      in
      match
        required r "protocol"
          (enum ~what:"protocol" [ ("command", `Command); ("http", `Http) ])
      with
      | Error errs ->
          (* The other fields depend on the protocol, so none can be judged. *)
          ignore_rest r;
          Error (errs @ match common with Ok _ -> [] | Error e -> e)
      | Ok `Command ->
          let+ argv = required r "command" (non_empty_list string)
          and+ () = forbidden r "url" ~protocol:"command"
          and+ () = forbidden r "namespace_header" ~protocol:"command"
          and+ (turn_timeout, total_timeout), env = common in
          (Command { argv; turn_timeout; total_timeout }, env)
      | Ok `Http ->
          let+ url = required r "url" non_empty_string
          and+ namespace_header = required r "namespace_header" non_empty_string
          and+ () = forbidden r "command" ~protocol:"http"
          and+ (turn_timeout, total_timeout), env = common in
          (Http { url; namespace_header; turn_timeout; total_timeout }, env))

let env_vars = assoc string

let variant_body ~path c =
  object_ ~path c (fun r -> default r "env" [] env_vars)

(* Mapping form: the key is the name, so duplicates cannot occur. *)
let variants_of_mapping = assoc variant_body

let variant_entry ~path c =
  object_ ~path c (fun r ->
      let+ name = required r "name" non_empty_string
      and+ env = default r "env" [] env_vars in
      { name; env })

let reject_duplicate_names ~path (variants : variant list) =
  let rec go seen i = function
    | [] -> Ok ()
    | (v : variant) :: rest ->
        if List.exists (String.equal v.name) seen then
          let dup =
            fail
              ~path:(field_path (index_path path i) "name")
              (Printf.sprintf "duplicate variant name %S" v.name)
          in
          let+ () = dup and+ () = go seen (i + 1) rest in
          ()
        else go (v.name :: seen) (i + 1) rest
  in
  go [] 0 variants

let variants ~path c =
  let* vs =
    match (c : Canon.t) with
    | Object _ ->
        let+ named = variants_of_mapping ~path c in
        List.map (fun (name, env) -> { name; env }) named
    | List _ -> list variant_entry ~path c
    | Null | Bool _ | Int _ | Big_int _ | Float _ | String _ ->
        fail ~path "expected a mapping of variants or a list of {name, env}"
  in
  match vs with
  | [] -> fail ~path "must contain at least one variant"
  | _ :: _ ->
      let+ () = reject_duplicate_names ~path vs in
      vs

let egress ~path c =
  object_ ~path c (fun r ->
      let+ default_policy = default r "default" Block policy
      and+ hosts = default r "hosts" [] (assoc policy) in
      (default_policy, hosts))

let intercept =
  list
    (enum ~what:"intercept target"
       [
         ("model", `Model);
         ("mcp", `Mcp);
         ("fixtures", `Fixtures);
         ("egress", `Egress);
       ])

let clock ~path c =
  object_ ~path c (fun r ->
      default r "faketime" true
        (enum ~what:"faketime mode" [ ("auto", true); ("off", false) ]))

let confidence ~path c =
  let* f = float ~path c in
  if f > 0.0 && f < 1.0 then Ok f
  else
    fail ~path (Printf.sprintf "must be strictly between 0 and 1, found %g" f)

let compare_block ~path c =
  object_ ~path c (fun r ->
      let+ baseline = required r "baseline" non_empty_string
      and+ confidence = default r "confidence" 0.95 confidence
      and+ thresholds = default r "thresholds" [] (assoc (float_at_least 0.0))
      and+ gate = default r "gate" [ "pass_rate" ] (list non_empty_string)
      and+ population =
        default r "population" `Suite
          (enum ~what:"population"
             [ ("suite", `Suite); ("scenarios", `Scenarios) ])
      in
      { baseline; confidence; thresholds; gate; population })

let sandbox =
  enum ~what:"sandbox mode"
    [ ("auto", `Auto); ("strict", `Strict); ("off", `Off) ]

let baseline_exists (variants : variant list) = function
  | None -> Ok ()
  | Some (c : compare) ->
      if
        List.exists
          (fun (v : variant) -> String.equal v.name c.baseline)
          variants
      then Ok ()
      else
        fail ~path:"compare.baseline"
          (Printf.sprintf "%S is not a variant (variants: %s)" c.baseline
             (String.concat ", "
                (List.map (fun (v : variant) -> v.name) variants)))

let decode c =
  let decoded =
    object_ ~path:"" c (fun r ->
        let+ _version = required r "version" (version 1)
        and+ name = required r "name" non_empty_string
        and+ agent, env_passthrough = required r "agent" agent
        and+ scenarios =
          required r "scenarios" (non_empty_list non_empty_string)
        and+ variants = required r "variants" variants
        and+ repeats = default r "repeats" 1 (int_at_least 1)
        and+ concurrency = default r "concurrency" 1 (int_at_least 1)
        and+ intercept =
          default r "intercept" [ `Model; `Mcp; `Fixtures; `Egress ] intercept
        and+ egress_default, egress_hosts =
          default r "egress" (Block, []) egress
        and+ mcp = default r "mcp" [] (assoc policy)
        and+ normalise = default r "normalise" [] (list regex)
        and+ redact = default r "redact" [] (list regex)
        and+ sandbox = default r "sandbox" `Auto sandbox
        and+ faketime = default r "clock" true clock
        and+ caps_usd = default r "caps_usd" [] (assoc (float_at_least 0.0))
        and+ pricing = optional r "pricing" non_empty_string
        and+ seed = default r "seed" 0 int
        and+ compare = optional r "compare" compare_block in
        {
          name;
          agent;
          scenarios;
          variants;
          repeats;
          concurrency;
          intercept;
          egress_default;
          egress_hosts;
          mcp;
          normalise;
          redact;
          sandbox;
          faketime;
          caps_usd;
          pricing;
          seed;
          env_passthrough;
          compare;
        })
  in
  let* t = decoded in
  let+ () = baseline_exists t.variants t.compare in
  t
