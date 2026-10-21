type outcome = (unit, string) result

type report = {
  public : outcome;
  listener : outcome;
  outside_write : outcome;
  inside_write : outcome;
  denied_read : outcome;
  masked_read : outcome;
  masked_case_read : outcome;
  root_write : outcome;
  other_run_write : outcome;
  no_new_privs : outcome;
  interfaces_listed : outcome;
  interfaces : string list;
}

type wire = {
  public : string;
  listener : string;
  outside_write : string;
  inside_write : string;
  denied_read : string;
  masked_read : string;
  masked_case_read : string;
  root_write : string;
  other_run_write : string;
  no_new_privs : string;
  interfaces_listed : string;
  interfaces : string list;
}
[@@deriving of_yojson]

let of_word w = if String.equal w "ok" then Ok () else Error w

let of_line line =
  match Yojson.Safe.from_string line with
  | exception Yojson.Json_error m -> Error m
  | json ->
      Result.map
        (fun (w : wire) : report ->
          {
            public = of_word w.public;
            listener = of_word w.listener;
            outside_write = of_word w.outside_write;
            inside_write = of_word w.inside_write;
            denied_read = of_word w.denied_read;
            masked_read = of_word w.masked_read;
            masked_case_read = of_word w.masked_case_read;
            root_write = of_word w.root_write;
            other_run_write = of_word w.other_run_write;
            no_new_privs = of_word w.no_new_privs;
            interfaces_listed = of_word w.interfaces_listed;
            interfaces = w.interfaces;
          })
        (wire_of_yojson json)

let checks ~listener ~outside_write ~inside_write ~denied_read =
  let must_work what = function
    | Ok () -> []
    | Error e -> [ Printf.sprintf "%s failed (%s)" what e ]
  in
  let must_fail what = function
    | Ok () -> [ what ^ " succeeded" ]
    | Error _ -> []
  in
  must_work "a connection to bactrian's own listener" listener
  @ must_fail "a write outside the state directory" outside_write
  @ must_work "a write inside the state directory" inside_write
  @ must_fail "a read of a denied file" denied_read

let judge (runtime : Experiment.sandbox_runtime) (r : report) =
  let public =
    match r.public with
    | Ok () -> [ "an outbound connection to 1.1.1.1:443 succeeded" ]
    | Error "timeout" ->
        [
          "an outbound connection to 1.1.1.1:443 timed out instead of failing \
           at once, so the container has a route out";
        ]
    | Error _ -> []
  in
  let interfaces =
    match
      ( r.interfaces_listed,
        List.filter (fun i -> not (String.equal i "lo")) r.interfaces )
    with
    | Error e, _ ->
        [ Printf.sprintf "the network interfaces could not be listed (%s)" e ]
    | Ok (), (_ :: _ as others) ->
        [
          "the container has network interfaces other than lo: "
          ^ String.concat ", " others;
        ]
    | Ok (), [] ->
        if List.exists (String.equal "lo") r.interfaces then []
        else [ "the container has no network interface at all, not even lo" ]
  in
  (* Each of these must fail, and with the one error that shows the mount
     doing it: anything else, a different error included, is a surprise. *)
  let fails_with what expected = function
    | Error e when String.equal e expected -> []
    | Ok () -> [ what ^ " succeeded" ]
    | Error e ->
        [ Printf.sprintf "%s failed with %s instead of %s" what e expected ]
  in
  let no_new_privs =
    match r.no_new_privs with
    | Ok () -> []
    | Error e ->
        [
          Printf.sprintf
            "no_new_privs is not set for the agent (%s), so a set-user-ID \
             program could raise its privileges"
            e;
        ]
  in
  match
    public @ interfaces @ no_new_privs
    @ fails_with "a read of a masked file" "ENOENT" r.masked_read
    @ fails_with "a read of a masked file under its name in another case"
        "ENOENT" r.masked_case_read
    @ fails_with "a write to /" "EROFS" r.root_write
    @ fails_with "a write into another run's directory" "ENOENT"
        r.other_run_write
    (* The shared checks below already report a success. *)
    (* Outside the state directory is under the mask over .bactrian: Docker's
       is read-only, container's is writable by root alone. *)
    @ (match r.outside_write with
      | Ok () -> []
      | Error _ as e ->
          fails_with "a write outside the state directory"
            (match runtime with `Docker -> "EROFS" | `Container -> "EACCES")
            e)
    @ checks ~listener:r.listener ~outside_write:r.outside_write
        ~inside_write:r.inside_write ~denied_read:r.denied_read
  with
  | [] -> Ok ()
  | wrong -> Error wrong
