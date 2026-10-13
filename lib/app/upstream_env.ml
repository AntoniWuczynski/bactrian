open Bactrian_runtime

let ( let* ) = Result.bind

let variables =
  List.map
    (fun (p, name) -> (p, "BACTRIAN_UPSTREAM_" ^ String.uppercase_ascii name))
    Run.providers

let strip_scheme raw =
  List.find_map
    (fun scheme ->
      let prefix = scheme ^ "://" in
      let n = String.length prefix in
      if
        String.length raw >= n
        && String.equal (String.lowercase_ascii (String.sub raw 0 n)) prefix
      then Some (scheme, String.sub raw n (String.length raw - n))
      else None)
    [ "http"; "https" ]

let name_char c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '_' -> true
  | _ -> false

let literal_char c =
  match c with
  | 'a' .. 'f' | 'A' .. 'F' | '0' .. '9' | ':' | '.' -> true
  | _ -> false

let all_chars f s = String.for_all f s

let valid_port text =
  text <> ""
  && all_chars (function '0' .. '9' -> true | _ -> false) text
  &&
  match int_of_string_opt text with
  | Some n -> n >= 1 && n <= 65535
  | None -> false

(* [host], [host:port], [[v6]] or [[v6]:port]. *)
let check_authority authority =
  let host_and_port =
    if String.starts_with ~prefix:"[" authority then
      match String.index_opt authority ']' with
      | None -> Error "has an unterminated IPv6 address"
      | Some close ->
          let host = String.sub authority 1 (close - 1) in
          let after =
            String.sub authority (close + 1)
              (String.length authority - close - 1)
          in
          if host <> "" && all_chars literal_char host then
            if after = "" then Ok (host, None)
            else if String.starts_with ~prefix:":" after then
              Ok (host, Some (String.sub after 1 (String.length after - 1)))
            else Error "has text after the IPv6 address that is not a port"
          else Error "has an invalid IPv6 address"
    else
      match String.rindex_opt authority ':' with
      | None -> Ok (authority, None)
      | Some i ->
          Ok
            ( String.sub authority 0 i,
              Some
                (String.sub authority (i + 1) (String.length authority - i - 1))
            )
  in
  let* host, port = host_and_port in
  let literal = String.starts_with ~prefix:"[" authority in
  let* () =
    if host = "" then Error "has no host"
    else if (not literal) && not (all_chars name_char host) then
      Error "has an invalid host"
    else Ok ()
  in
  match port with
  | Some p when not (valid_port p) ->
      Error "has a port that is not a number from 1 to 65535"
  | Some _ | None -> Ok ()

let check raw =
  if String.exists (fun c -> c <= ' ' || c = '\127') raw then
    Error "contains whitespace or a control character"
  else if String.contains raw '?' || String.contains raw '#' then
    Error "has a query or a fragment"
  else
    match strip_scheme raw with
    | None -> Error "is not an absolute http or https URL"
    | Some (scheme, rest) ->
        let authority, path =
          match String.index_opt rest '/' with
          | Some i ->
              (String.sub rest 0 i, String.sub rest i (String.length rest - i))
          | None -> (rest, "")
        in
        if String.contains authority '@' then
          Error "has user information (credentials) before the host"
        else
          let* () = check_authority authority in
          Ok (scheme ^ "://" ^ authority ^ path)

let parse ~variable raw =
  match check raw with
  | Ok url -> Ok (Uri.of_string url)
  | Error reason ->
      Error
        (Printf.sprintf
           "%s %s: it must be an absolute http or https URL with a host, an \
            optional port and an optional path prefix, and no user \
            information, query or fragment"
           variable reason)

let read getenv =
  List.fold_left
    (fun acc (p, variable) ->
      let* overrides = acc in
      match getenv variable with
      | None | Some "" -> Ok overrides
      | Some raw ->
          let* url = parse ~variable raw in
          Ok ((p, url) :: overrides))
    (Ok []) variables
  |> Result.map (fun overrides p ->
      match List.assoc_opt p overrides with
      | Some url -> url
      | None -> Model_proxy.default_upstream p)

let from_environment () = read Sys.getenv_opt
