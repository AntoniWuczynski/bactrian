open Cmdliner

let experiment_id =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"EXPERIMENT-ID"
        ~doc:"An experiment under ./.bactrian/experiments.")

let baseline_flag =
  Arg.(
    value
    & opt (some string) None
    & info [ "baseline" ] ~docv:"VARIANT"
        ~doc:
          "The baseline variant. Defaults to $(b,compare.baseline) in the \
           experiment file the manifest records.")

let candidate_flag =
  Arg.(
    value
    & opt (some string) None
    & info [ "candidate" ] ~docv:"VARIANT"
        ~doc:
          "The candidate variant. Defaults to the other variant when the \
           experiment has exactly two.")

let comparison_man =
  [
    `S Manpage.s_description;
    `P
      "Compares the candidate with the baseline over the runs of \
       $(i,EXPERIMENT-ID). The estimand, confidence, gate and thresholds come \
       from the $(b,compare) section of the experiment file that the manifest \
       records. Every gate metric needs a threshold there, under \
       $(b,compare.thresholds): none is ever assumed, and a missing one is a \
       usage error naming the metric. The file must therefore still exist and \
       have a $(b,compare) section. $(b,--baseline) and $(b,--candidate) \
       override its baseline and pick the candidate.";
  ]
