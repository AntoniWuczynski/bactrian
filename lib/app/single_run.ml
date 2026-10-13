open Bactrian_runtime
module P = Model_proxy

let ( let* ) = Result.bind

type common = {
  agent : string list;
  cwd : string;
  scenario : string;
  variant : string;
  sim_now : string;
  timezone : string;
  env_passthrough : string list;
  turn_timeout : float;
  total_timeout : float;
  sandbox : Sandbox.mode;
}

let defaults =
  {
    agent = [];
    cwd = ".";
    scenario = "adhoc";
    variant = "default";
    sim_now = "";
    timezone = "UTC";
    env_passthrough = [];
    turn_timeout = 120.;
    total_timeout = 600.;
    sandbox = `Auto;
  }

let positive name v =
  if Float.is_finite v && v > 0. then Ok ()
  else Error (Printf.sprintf "%s must be a positive number of seconds" name)

let positive_timeouts c =
  let* () = positive "--turn-timeout" c.turn_timeout in
  positive "--total-timeout" c.total_timeout

let config c ~turns ~replay ~mcp sandbox : Run.config =
  {
    agent = Command c.agent;
    cwd = c.cwd;
    turns;
    scenario = c.scenario;
    variant = c.variant;
    sim_now = c.sim_now;
    timezone = c.timezone;
    replay;
    env_passthrough = c.env_passthrough;
    variant_env = [];
    fixtures = None;
    normalise = [];
    redact = [];
    pricing = Bactrian_core.Pricing.default;
    checks = None;
    unenforced = [];
    turn_timeout = c.turn_timeout;
    total_timeout = c.total_timeout;
    mcp;
    sandbox;
    faketime = true;
  }

let cap_variable name = "BACTRIAN_CAP_" ^ String.uppercase_ascii name

let read_caps () =
  List.fold_left
    (fun acc (_, name) ->
      Result.bind acc (fun caps ->
          match Sys.getenv_opt (cap_variable name) with
          | None | Some "" -> Ok ((name, 0.) :: caps)
          | Some raw -> (
              match float_of_string_opt raw with
              | Some usd when Float.is_finite usd && usd >= 0. ->
                  Ok ((name, usd) :: caps)
              | Some _ | None ->
                  Error
                    (Printf.sprintf "%s=%S is not a non-negative amount in USD"
                       (cap_variable name) raw))))
    (Ok []) Run.providers
  |> Result.map List.rev

(* A live call needs a spend cap above zero, the key behind it and, for an
   https upstream, TLS. *)
let live_problem ~upstream caps =
  Option.map
    (Live_preflight.describe
       ~set_caps:
         (Printf.sprintf "%s (USD) for the providers this run calls"
            (String.concat ", "
               (List.map (fun (_, n) -> cap_variable n) Run.providers))))
    (Live_preflight.problem ~caps
       ~has_key:(fun p ->
         match Sys.getenv_opt (Run.key_variable p) with
         | None | Some "" -> false
         | Some _ -> true)
       ~upstream ~tls_available:Features.tls_available)

let execute ?stop ~on_progress ~bactrian_exe ~env ~live ~sandbox config_of =
  let warn w = on_progress (Progress.Warning w) in
  match (read_caps (), Upstream_env.from_environment ()) with
  | Error m, _ | _, Error m -> Error (Command_error.Usage m)
  | Ok caps, Ok upstream -> (
      match if live then live_problem ~upstream caps else None with
      | Some m -> Error (Command_error.Environment m)
      | None -> (
          if live then List.iter warn (Live_preflight.cleartext caps upstream);
          Mirage_crypto_rng_unix.use_default ();
          Eio.Switch.run @@ fun sw ->
          let root = Eio.Path.(Eio.Stdenv.cwd env / ".bactrian") in
          match Ledger.create ~path:Eio.Path.(root / "ledger.jsonl") ~caps with
          | exception Ledger.Corrupt { line; reason } ->
              Error
                (Command_error.Environment
                   (Printf.sprintf
                      "the spend ledger .bactrian/ledger.jsonl is corrupt \
                       (line %d: %s)"
                      line reason))
          | ledger -> (
              match
                let* sandbox =
                  Sandbox.preflight ~env ~warn
                    {
                      mode = sandbox;
                      allow_write = [];
                      deny_read = [];
                      bactrian_exe;
                      bactrian_dir = Eio.Path.native_exn root;
                    }
                in
                let* proxy =
                  P.start ~sw ~net:(Eio.Stdenv.net env)
                    ~clock:(Eio.Stdenv.clock env)
                    ~process_mgr:(Eio.Stdenv.process_mgr env)
                    ()
                in
                Ok (sandbox, proxy)
              with
              | Error m -> Error (Command_error.Environment m)
              | Ok (sandbox, proxy) -> (
                  let store = Store.create ~root in
                  match
                    Interrupt.run ?stop (fun () ->
                        Result.bind (config_of store sandbox) (fun config ->
                            Option.iter warn
                              (Sandbox.mcp_warning sandbox
                                 (config : Run.config).mcp);
                            Run.execute ~upstream ~env ~proxy ~store ~ledger
                              config))
                  with
                  | Error signal ->
                      Error
                        (Command_error.Interrupted
                           { signal; experiment_id = None })
                  | Ok (Error m) -> Error (Command_error.Usage m)
                  | Ok (Ok result) -> Ok result))))

let exit_code (r : Run.result) = Exit_code.of_outcome r.outcome

let to_json (r : Run.result) =
  let f = r.findings in
  `Assoc
    [
      ("run_id", `String r.run_id);
      ("run_dir", `String r.run_dir);
      ( "outcome",
        `Assoc
          (("kind", `String (Run.outcome_kind r.outcome))
          ::
          (match Run.outcome_reason r.outcome with
          | Some reason -> [ ("reason", `String reason) ]
          | None -> [])) );
      ( "divergences",
        `List
          (List.map
             (fun d -> Divergences.to_json (Divergences.of_matcher d))
             f.divergences) );
      ( "findings",
        `Assoc
          [
            ("exchanges", `Int f.exchanges);
            ("transport_errors", `Int f.transport_errors);
            ("usage_gaps", `Int f.usage_gaps);
            ("refused_keys", `Int f.refused_keys);
            ("cap_refusals", `Int (List.length f.cap_refusals));
            ("errors", `List (List.map (fun e -> `String e) f.errors));
          ] );
      ("spend_usd", `Float r.spend);
      ("sandbox", Sandbox.label_json r.sandbox);
      ("clock", Bactrian_core.Clock_label.to_json r.clock);
      ("sandbox_violations", Sandbox.violations_json r.violations);
    ]
