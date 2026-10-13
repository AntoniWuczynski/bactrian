type request = {
  experiment_id : string;
  baseline : string option;
  candidate : string option;
  fail_on_inconclusive : bool;
}

let run ?(compute = Compute.inline) ~env r =
  Comparison.analyse ~compute ~env ~experiment_id:r.experiment_id
    ~baseline:r.baseline ~candidate:r.candidate
    ~fail_on_inconclusive:r.fail_on_inconclusive

let to_json = Bactrian_core.Report_data.summary_json
