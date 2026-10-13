open Bactrian_core

type t = {
  experiment : string;
  agent : string;
  pricing : string option;
  scenarios : (string * string) list;
}

let ( let* ) = Result.bind
let digest text = Digest.BLAKE256.(to_hex (string text))

let read path =
  match Eio.Path.load path with
  | text -> Ok text
  | exception Eio.Io _ -> Error (Printf.sprintf "cannot read %s" (snd path))

let compute ~base ~experiment ~pricing_file ~scenario_files =
  let agent =
    Option.value ~default:Canon.null (Canon.member "agent" experiment)
  in
  let rest =
    List.fold_left
      (fun doc key -> Canon.remove_member key doc)
      experiment
      [ "agent"; "caps_usd"; "concurrency" ]
  in
  let* pricing =
    match pricing_file with
    | None -> Ok None
    | Some file ->
        let* text = read Eio.Path.(base / file) in
        Ok (Some (digest text))
  in
  let* scenarios =
    List.fold_right
      (fun file acc ->
        let* rest = acc in
        let* text = read Eio.Path.(base / file) in
        Ok ((file, digest text) :: rest))
      scenario_files (Ok [])
  in
  Ok
    {
      experiment = Canon.digest rest;
      agent = Canon.digest agent;
      pricing;
      scenarios;
    }

let changes ~recorded t =
  List.concat
    [
      (if String.equal recorded.experiment t.experiment then []
       else [ "the experiment changed (anything but caps_usd and concurrency)" ]);
      (if String.equal recorded.agent t.agent then []
       else [ "the agent changed" ]);
      (if Option.equal String.equal recorded.pricing t.pricing then []
       else [ "the price file changed" ]);
      List.filter_map
        (fun (file, d) ->
          match List.assoc_opt file recorded.scenarios with
          | Some old when not (String.equal old d) ->
              Some (Printf.sprintf "scenario %s changed" file)
          | Some _ | None -> None)
        t.scenarios;
    ]

type wire_scenario = { path : string; digest : string }
[@@deriving yojson { strict = true }]

type wire = {
  experiment : string;
  agent : string;
  pricing : string option; [@default None]
  scenarios : wire_scenario list;
}
[@@deriving yojson { strict = true }]

let to_yojson (t : t) =
  wire_to_yojson
    {
      experiment = t.experiment;
      agent = t.agent;
      pricing = t.pricing;
      scenarios = List.map (fun (path, digest) -> { path; digest }) t.scenarios;
    }

let of_yojson json =
  Result.map
    (fun (w : wire) : t ->
      {
        experiment = w.experiment;
        agent = w.agent;
        pricing = w.pricing;
        scenarios =
          List.map (fun (s : wire_scenario) -> (s.path, s.digest)) w.scenarios;
      })
    (wire_of_yojson json)
