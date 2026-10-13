val to_json : float -> Yojson.Safe.t
(** A statistic for JSON output: rounded to 12 significant digits so that the
    last-place differences between platform maths libraries never reach the
    output, negative zero written as zero, and non-finite values as [null]. *)
