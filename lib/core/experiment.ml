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

type mcp_transport =
  | Mcp_stdio of {
      argv : string list;
      env : (string * string) list;
      cwd : string option;
    }
  | Mcp_http of { url : string }

type mcp_server = { transport : mcp_transport; policy : policy }

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
  mcp : (string * mcp_server) list;
  normalise : string list;
  redact : string list;
  sandbox : [ `Auto | `Strict | `Off ];
  sandbox_allow_write : string list;
  sandbox_deny_read : string list;
  faketime : bool;
  caps_usd : (string * float) list;
  pricing : string option;
  seed : int;
  env_passthrough : string list;
  compare : compare option;
}

type error = Scenario.error

let policies =
  [
    ("record", Record);
    ("replay", Replay);
    ("passthrough", Passthrough);
    ("block", Block);
  ]

let policy_name = function
  | Record -> "record"
  | Replay -> "replay"
  | Passthrough -> "passthrough"
  | Block -> "block"

let policy_of_name name =
  List.find_map
    (fun (n, p) -> if String.equal n name then Some p else None)
    policies

let policy = enum ~what:"policy" policies

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

(* An experiment's runs all record, so a server cannot be served from a
   recording that does not exist. *)
let mcp_policy ~path c =
  let* p = policy ~path c in
  match p with
  | Replay ->
      fail ~path
        "replay needs a recording and an experiment's runs record: use record \
         or passthrough"
  | Record | Passthrough | Block -> Ok p

let valid_mcp_url url =
  let after prefix =
    String.starts_with ~prefix url && String.length url > String.length prefix
  in
  after "http://" || after "https://"

let mcp_url ~path c =
  let* url = non_empty_string ~path c in
  if valid_mcp_url url then Ok url
  else fail ~path "expected an http:// or https:// URL"

let mcp_server ~path c =
  object_ ~path c (fun r ->
      let+ policy = default r "policy" Record mcp_policy
      and+ transport =
        match
          ( optional r "command" (non_empty_list string),
            optional r "url" mcp_url )
        with
        | Ok (Some argv), Ok None ->
            let+ env = default r "env" [] env_vars
            and+ cwd = optional r "cwd" non_empty_string in
            Mcp_stdio { argv; env; cwd }
        | Ok None, Ok (Some url) ->
            let+ () = forbidden r "env" ~protocol:"http"
            and+ () = forbidden r "cwd" ~protocol:"http" in
            Mcp_http { url }
        | Ok (Some _), Ok (Some _) ->
            fail ~path:(Config_decode.path r)
              "command and url are mutually exclusive"
        | Ok None, Ok None ->
            fail ~path:(Config_decode.path r)
              "needs a command (stdio) or a url (HTTP)"
        | Error e, Ok _ | Ok _, Error e -> Error e
        | Error e, Error e' -> Error (e @ e')
      in
      { transport; policy })

(* The names become URL path segments and, for HTTP servers, environment
   variable suffixes. *)
let mcp_http_prefix = "BACTRIAN_MCP_HTTP_"

let mcp_http_variable name =
  mcp_http_prefix
  ^ String.map
      (function
        | ('A' .. 'Z' | '0' .. '9') as c -> c
        | 'a' .. 'z' as c -> Char.uppercase_ascii c
        | _ -> '_')
      name

let valid_mcp_name name =
  name <> ""
  && String.for_all
       (function
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true | _ -> false)
       name

let mcp_http_clash ~http_names name =
  let v = mcp_http_variable name in
  if
    List.exists
      (fun other ->
        (not (String.equal other name))
        && String.equal v (mcp_http_variable other))
      http_names
  then Some v
  else None

let mcp_servers ~path c =
  let* servers = assoc mcp_server ~path c in
  let http_names =
    List.filter_map
      (fun (name, s) ->
        match s.transport with Mcp_http _ -> Some name | Mcp_stdio _ -> None)
      servers
  in
  let check (name, s) =
    let at message = fail ~path:(field_path path name) message in
    if not (valid_mcp_name name) then
      at "a server name has letters, digits, '_' and '-' only"
    else
      match s.transport with
      | Mcp_stdio _ -> Ok ()
      | Mcp_http _ -> (
          match mcp_http_clash ~http_names name with
          | Some v ->
              at (Printf.sprintf "%s would also be another server's variable" v)
          | None -> Ok ())
  in
  let+ (_ : unit list) = collect (List.map check servers) in
  servers

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
  let* vs, mapping_form =
    match (c : Canon.t) with
    | Object _ ->
        let+ named = variants_of_mapping ~path c in
        (List.map (fun (name, env) -> { name; env }) named, true)
    | List _ ->
        let+ entries = list variant_entry ~path c in
        (entries, false)
    | Null | Bool _ | Int _ | Big_int _ | Float _ | String _ ->
        fail ~path "expected a mapping of variants or a list of {name, env}"
  in
  match vs with
  | [] -> fail ~path "must contain at least one variant"
  | _ :: _ ->
      let+ () = reject_duplicate_names ~path vs in
      (vs, mapping_form)

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

(* A pass-rate threshold is an absolute difference, so it lies in (0, 1).
   The others are relative changes, above 0. A threshold of 0 would ask for
   certainty about an effect of exactly nothing. *)
let threshold ~pass_rate_path ~path c =
  let* f = float ~path c in
  if String.equal path pass_rate_path then
    if f > 0.0 && f < 1.0 then Ok f
    else
      fail ~path
        (Printf.sprintf
           "a pass-rate threshold must be strictly between 0 and 1, found %g" f)
  else if f > 0.0 then Ok f
  else fail ~path (Printf.sprintf "must be above 0, found %g" f)

let compare_block ~path c =
  let pass_rate_path = field_path (field_path path "thresholds") "pass_rate" in
  object_ ~path c (fun r ->
      let+ baseline = required r "baseline" non_empty_string
      and+ confidence = default r "confidence" 0.95 confidence
      and+ thresholds =
        default r "thresholds" [] (assoc (threshold ~pass_rate_path))
      and+ gate = default r "gate" [ "pass_rate" ] (list non_empty_string)
      and+ population =
        default r "population" `Suite
          (enum ~what:"population"
             [ ("suite", `Suite); ("scenarios", `Scenarios) ])
      in
      { baseline; confidence; thresholds; gate; population })

let caps_usd ~path c =
  let names =
    Option.fold ~none:[] ~some:(List.map fst) (Canon.to_fields_opt c)
  in
  let known name =
    if List.exists (String.equal name) Pricing.providers then Ok ()
    else
      fail ~path:(field_path path name)
        (Printf.sprintf "unknown provider %S (expected one of: %s)" name
           (String.concat ", " Pricing.providers))
  in
  let+ caps = assoc (float_at_least 0.0) ~path c
  and+ (_ : unit list) = collect (List.map known names) in
  caps

let sandbox_mode =
  enum ~what:"sandbox mode"
    [ ("auto", `Auto); ("strict", `Strict); ("off", `Off) ]

let sandbox ~path c =
  match (c : Canon.t) with
  | String _ ->
      let+ mode = sandbox_mode ~path c in
      (mode, [], [])
  | Object _ ->
      object_ ~path c (fun r ->
          let+ mode = default r "mode" `Auto sandbox_mode
          and+ allow_write = default r "allow_write" [] (list non_empty_string)
          and+ deny_read = default r "deny_read" [] (list non_empty_string) in
          (mode, allow_write, deny_read))
  | Null | Bool _ | Int _ | Big_int _ | Float _ | List _ ->
      fail ~path
        "expected auto, strict, off or a mapping {mode, allow_write, deny_read}"

let reserved_env =
  [
    "SIM_NOW";
    "TZ";
    "BACTRIAN_RUN_ID";
    "BACTRIAN_STATE_DIR";
    "BACTRIAN_MCP_URL";
    "OPENAI_BASE_URL";
    "ANTHROPIC_BASE_URL";
    "GOOGLE_GEMINI_BASE_URL";
    "OPENROUTER_BASE_URL";
    "OPENAI_API_KEY";
    "ANTHROPIC_API_KEY";
    "GEMINI_API_KEY";
    "OPENROUTER_API_KEY";
    "HOME";
    "XDG_CACHE_HOME";
    "XDG_CONFIG_HOME";
    "XDG_DATA_HOME";
    "npm_config_cache";
    "TMPDIR";
    "PYTHONDONTWRITEBYTECODE";
  ]

let reserved_env_prefixes = [ mcp_http_prefix ]

let is_reserved_env key =
  List.mem key reserved_env
  || List.exists
       (fun prefix -> String.starts_with ~prefix key)
       reserved_env_prefixes

(* [named] says how the variant is written in the document, as a path. *)
let env_not_reserved ~mapping_form (variants : variant list) =
  collect
    (List.mapi
       (fun i (v : variant) ->
         let base =
           if mapping_form then field_path "variants" v.name
           else index_path "variants" i
         in
         collect
           (List.filter_map
              (fun (key, _) ->
                if is_reserved_env key then
                  Some
                    (fail
                       ~path:(field_path (field_path base "env") key)
                       (Printf.sprintf
                          "%s is set by bactrian for every run and cannot be \
                           overridden"
                          key))
                else None)
              v.env))
       variants)
  |> Result.map ignore

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
  object_ ~path:"" c (fun r ->
      let variants_r = required r "variants" variants in
      let compare = optional r "compare" compare_block in
      (* Checks that need two decoded parts run beside the others, so their
         errors come out in the same round. *)
      let cross_checks =
        match (variants_r, compare) with
        | Ok (vs, mapping_form), Ok cmp ->
            let+ () = baseline_exists vs cmp
            and+ () = env_not_reserved ~mapping_form vs in
            ()
        | Ok (vs, mapping_form), Error _ -> env_not_reserved ~mapping_form vs
        | Error _, (Ok _ | Error _) -> Ok ()
      in
      let+ _version = required r "version" (version 1)
      and+ name = required r "name" non_empty_string
      and+ agent, env_passthrough = required r "agent" agent
      and+ scenarios = required r "scenarios" (non_empty_list non_empty_string)
      and+ variants, _ = variants_r
      and+ repeats = default r "repeats" 1 (int_at_least 1)
      and+ concurrency = default r "concurrency" 1 (int_at_least 1)
      and+ intercept =
        default r "intercept" [ `Model; `Mcp; `Fixtures; `Egress ] intercept
      and+ egress_default, egress_hosts = default r "egress" (Block, []) egress
      and+ mcp = default r "mcp" [] mcp_servers
      and+ normalise = default r "normalise" [] (list regex)
      and+ redact = default r "redact" [] (list regex)
      and+ sandbox, sandbox_allow_write, sandbox_deny_read =
        default r "sandbox" (`Auto, [], []) sandbox
      and+ faketime = default r "clock" true clock
      and+ caps_usd = default r "caps_usd" [] caps_usd
      and+ pricing = optional r "pricing" non_empty_string
      and+ seed = default r "seed" 0 int
      and+ compare = compare
      and+ () = cross_checks in
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
        sandbox_allow_write;
        sandbox_deny_read;
        faketime;
        caps_usd;
        pricing;
        seed;
        env_passthrough;
        compare;
      })

let unenforced t =
  List.concat
    [
      (if List.mem `Egress t.intercept then
         [
           "egress: the agent's network access is neither blocked nor recorded";
         ]
       else []);
      (match t.mcp with
      | _ :: _ when not (List.mem `Mcp t.intercept) ->
          [
            "mcp: servers are declared but intercept leaves out mcp, so they \
             are not started and the agent is not told about them";
          ]
      | _ :: _ | [] -> []);
      (if t.faketime then
         [
           "clock: faketime auto is not applied (SIM_NOW and TZ are a contract \
            only)";
         ]
       else []);
    ]
