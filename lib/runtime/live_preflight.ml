type problem =
  | No_cap
  | Missing_key of { provider : Model_proxy.provider; name : string }
  | No_tls of { providers : string list; reason : string }

let funded caps =
  List.filter
    (fun (_, name) ->
      match List.assoc_opt name caps with Some usd -> usd > 0. | None -> false)
    Run.providers

let https_funded caps upstream =
  List.filter_map
    (fun (p, name) ->
      if Upstream.is_https (upstream p) then Some name else None)
    (funded caps)

let problem ~caps ~has_key ~upstream ~tls =
  match funded caps with
  | [] -> Some No_cap
  | funded -> (
      match List.find_opt (fun (p, _) -> not (has_key p)) funded with
      | Some (provider, name) -> Some (Missing_key { provider; name })
      | None -> (
          match https_funded caps upstream with
          | [] -> None
          | _ :: _ as providers -> (
              match tls () with
              | Ok (_ : Upstream.trust) -> None
              | Error reason -> Some (No_tls { providers; reason }))))

let describe ~set_caps = function
  | No_cap -> "no provider has a spend cap above 0: set " ^ set_caps
  | Missing_key { provider; name } ->
      Printf.sprintf "%s has a spend cap but %s is not set" name
        (Run.key_variable provider)
  | No_tls { providers; reason } ->
      Printf.sprintf "%s, so bactrian cannot call providers (%s) over HTTPS"
        reason
        (String.concat ", " providers)

let is_loopback uri =
  match Uri.host uri with
  | None -> false
  | Some "localhost" -> true
  | Some host -> (
      let host =
        if
          String.starts_with ~prefix:"[" host
          && String.ends_with ~suffix:"]" host
        then String.sub host 1 (String.length host - 2)
        else host
      in
      match Unix.inet_addr_of_string host with
      | exception Failure _ -> false
      | addr ->
          let text = Unix.string_of_inet_addr addr in
          String.starts_with ~prefix:"127." text
          || String.equal text
               (Unix.string_of_inet_addr Unix.inet6_addr_loopback)
          || String.starts_with ~prefix:"::ffff:127." text)

let cleartext caps upstream =
  List.filter_map
    (fun (p, _) ->
      let url = upstream p in
      if Upstream.is_https url || is_loopback url then None
      else
        Some
          (Printf.sprintf
             "the real %s goes to %s unencrypted: if that server needs no key, \
              set %s to any placeholder"
             (Run.key_variable p) (Uri.to_string url) (Run.key_variable p)))
    (funded caps)

let mcp_https ~replaying (configs : Mcp_proxy.config list) =
  List.filter_map
    (fun ({ name; server; policy } : Mcp_proxy.config) ->
      match server with
      | Stdio _ -> None
      | Http { url } ->
          let live =
            match policy with
            | Passthrough -> true
            | Record -> not replaying
            | Replay | Block -> false
          in
          if live && Upstream.is_https url then
            Some
              (Printf.sprintf "the MCP server %s at %s" name (Uri.to_string url))
          else None)
    configs

let https_refusal ~tls = function
  | [] -> None
  | _ :: _ as needs -> (
      match tls () with
      | Ok (_ : Upstream.trust) -> None
      | Error reason ->
          Some
            (Printf.sprintf "%s, so bactrian cannot call %s over HTTPS" reason
               (String.concat ", " needs)))

let extra_trust ({ variable; count } : Upstream.extra_trust) =
  Printf.sprintf
    "%s adds %d certificate%s trusted for every HTTPS host, the providers \
     included"
    variable count
    (if count = 1 then "" else "s")

let trust_warning ~tls = function
  | [] -> None
  | _ :: _ -> (
      match tls () with
      | Ok ({ extra = Some e; _ } : Upstream.trust) -> Some (extra_trust e)
      | Ok { extra = None; _ } | Error (_ : string) -> None)
