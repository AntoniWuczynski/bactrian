let ( let* ) = Option.bind

let digits s pos len =
  let rec go i acc =
    if i = len then Some acc
    else
      match s.[pos + i] with
      | '0' .. '9' as c -> go (i + 1) ((acc * 10) + Char.code c - Char.code '0')
      | _ -> None
  in
  if pos + len <= String.length s then go 0 0 else None

let leap y = (y mod 4 = 0 && y mod 100 <> 0) || y mod 400 = 0

let days_in_month y = function
  | 2 -> if leap y then 29 else 28
  | 4 | 6 | 9 | 11 -> 30
  | _ -> 31

(* Days since 1970-01-01 of a proleptic Gregorian date. *)
let days_from_civil y m d =
  let y = if m <= 2 then y - 1 else y in
  let era = (if y >= 0 then y else y - 399) / 400 in
  let yoe = y - (era * 400) in
  let doy = (((153 * if m > 2 then m - 3 else m + 9) + 2) / 5) + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let separator s pos chars =
  if pos < String.length s && List.mem s.[pos] chars then Some () else None

(* The offset in seconds east of UTC, read from [pos] to the end of [s]. *)
let offset s pos =
  let n = String.length s in
  let rec skip_fraction p =
    if p < n && s.[p] >= '0' && s.[p] <= '9' then skip_fraction (p + 1) else p
  in
  let* pos =
    if pos < n && s.[pos] = '.' then
      let stop = skip_fraction (pos + 1) in
      if stop > pos + 1 then Some stop else None
    else Some pos
  in
  if pos >= n then None
  else
    match s.[pos] with
    | 'Z' | 'z' -> if pos + 1 = n then Some 0 else None
    | ('+' | '-') as sign ->
        if pos + 6 <> n then None
        else
          let* () = separator s (pos + 3) [ ':' ] in
          let* h = digits s (pos + 1) 2 in
          let* m = digits s (pos + 4) 2 in
          if h > 23 || m > 59 then None
          else
            let seconds = (h * 3600) + (m * 60) in
            Some (if sign = '-' then -seconds else seconds)
    | _ -> None

let epoch s =
  let* year = digits s 0 4 in
  let* () = separator s 4 [ '-' ] in
  let* month = digits s 5 2 in
  let* () = separator s 7 [ '-' ] in
  let* day = digits s 8 2 in
  let* () = separator s 10 [ 'T'; 't' ] in
  let* hour = digits s 11 2 in
  let* () = separator s 13 [ ':' ] in
  let* minute = digits s 14 2 in
  let* () = separator s 16 [ ':' ] in
  let* second = digits s 17 2 in
  let* east = offset s 19 in
  if
    month < 1 || month > 12 || day < 1
    || day > days_in_month year month
    || hour > 23 || minute > 59 || second > 59
  then None
  else
    Some
      ((days_from_civil year month day * 86400)
      + (hour * 3600) + (minute * 60) + second - east)
