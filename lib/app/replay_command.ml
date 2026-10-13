open Bactrian_runtime

let ( let* ) = Result.bind

type mode = Strict | Until_divergence | Fuzzy

type request = {
  common : Single_run.common;
  from : string;
  mode : mode;
  threshold : float option;
  allow_reorder : bool;
  timing : Model_proxy.timing;
}

let matcher r : (Bactrian_core.Matcher.mode, string) result =
  match (r.mode, r.threshold) with
  | Strict, None -> Ok Strict
  | Until_divergence, None -> Ok Until_divergence
  | Fuzzy, None -> Ok (Fuzzy 0.9)
  | Fuzzy, Some t when Float.is_finite t && t >= 0. && t <= 1. -> Ok (Fuzzy t)
  | Fuzzy, Some _ -> Error "--threshold must be between 0 and 1"
  | (Strict | Until_divergence), Some _ ->
      Error "--threshold only applies to --mode fuzzy"

let prepare r =
  let* matcher = matcher r in
  let* () = Single_run.positive_timeouts r.common in
  Ok matcher

let validate r = Result.map ignore (prepare r)

let run ?stop ~on_progress ~bactrian_exe ~env r =
  match prepare r with
  | Error m -> Error (Command_error.Usage m)
  | Ok matcher ->
      let live =
        match matcher with
        | Until_divergence -> true
        | Strict | Fuzzy _ -> false
      in
      Single_run.execute ?stop ~on_progress ~bactrian_exe ~env ~live
        ~sandbox:r.common.sandbox (fun store sandbox ->
          let* mcp = Run.recorded_mcp store ~from:r.from in
          Ok
            (Single_run.config r.common ~turns:[]
               ~replay:
                 (Some
                    {
                      Run.from = r.from;
                      matcher;
                      allow_reorder = r.allow_reorder;
                      timing = r.timing;
                    })
               ~mcp sandbox))

let exit_code = Single_run.exit_code
let to_json = Single_run.to_json
