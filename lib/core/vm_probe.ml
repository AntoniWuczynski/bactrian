type outcome = (unit, string) result

type report = {
  public : outcome;
  listener : outcome;
  outside_write : outcome;
  inside_write : outcome;
  denied_read : outcome;
  interfaces : string list;
}

type wire = {
  public : string;
  listener : string;
  outside_write : string;
  inside_write : string;
  denied_read : string;
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

let judge (r : report) =
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
    match List.filter (fun i -> not (String.equal i "lo")) r.interfaces with
    | [] -> []
    | others ->
        [
          "the container has network interfaces other than lo: "
          ^ String.concat ", " others;
        ]
  in
  match
    public @ interfaces
    @ checks ~listener:r.listener ~outside_write:r.outside_write
        ~inside_write:r.inside_write ~denied_read:r.denied_read
  with
  | [] -> Ok ()
  | wrong -> Error wrong
