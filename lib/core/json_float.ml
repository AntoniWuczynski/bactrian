let to_json f : Yojson.Safe.t =
  if not (Float.is_finite f) then `Null
  else
    let rounded =
      Option.value ~default:f (float_of_string_opt (Printf.sprintf "%.12g" f))
    in
    `Float (if Float.equal rounded 0. then 0. else rounded)
