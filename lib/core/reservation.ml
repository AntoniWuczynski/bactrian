type t = { external_input : bool; candidates : int; fallbacks : string list }

(* Members that point at server-side state or files. *)
let reference_members =
  [
    "previous_response_id";
    "previous_interaction_id";
    "conversation";
    "cachedContent";
    "cached_content";
    "file_id";
    "file_data";
    "fileData";
    "file_uri";
    "fileUri";
    "inlineData";
    "inline_data";
    "image_url";
    "input_audio";
    "input_image";
    "input_file";
  ]

(* Values of a [type] member that mark a media or file part. *)
let media_types =
  [
    "image";
    "image_url";
    "input_image";
    "audio";
    "input_audio";
    "video";
    "input_video";
    "file";
    "input_file";
    "document";
  ]

let is_null v = Canon.equal v Canon.null

let rec references (v : Canon.t) =
  match Canon.to_fields_opt v with
  | Some fields ->
      List.exists
        (fun (k, child) ->
          (List.mem k reference_members && not (is_null child))
          || String.equal k "type"
             && Option.fold ~none:false
                  ~some:(fun t -> List.mem t media_types)
                  (Canon.to_string_opt child)
          || references child)
        fields
  | None -> (
      match Canon.to_list_opt v with
      | Some items -> List.exists references items
      | None -> false)

let rec int_at v = function
  | [] -> Canon.to_int_opt v
  | k :: rest -> Option.bind (Canon.member k v) (fun c -> int_at c rest)

let candidate_paths =
  [
    [ "n" ];
    [ "generationConfig"; "candidateCount" ];
    [ "generation_config"; "candidate_count" ];
  ]

let analyse body =
  let candidates =
    List.fold_left
      (fun acc path ->
        match int_at body path with Some n -> max acc n | None -> acc)
      1 candidate_paths
  in
  let fallbacks =
    Option.value (Canon.list_member "models" body) ~default:[]
    |> List.filter_map Canon.to_string_opt
  in
  { external_input = references body; candidates; fallbacks }

(* Every model the request may be served by. [None] as soon as one is unknown
   to [f]. *)
let max_over ~model body f =
  let models = model :: (analyse body).fallbacks in
  List.fold_left
    (fun acc m ->
      match (acc, f m) with
      | Some a, Some c -> Some (Float.max a c)
      | None, _ | _, None -> None)
    (Some 0.) models

let worst_case ~model ~date ~input_bytes ~max_output body =
  let { external_input; candidates; fallbacks = _ } = analyse body in
  max_over ~model body (fun model ->
      Pricing.worst_case ~model ~date ~input_bytes ~external_input ~candidates
        ~max_output)

let cost ~model ~date body usage =
  max_over ~model body (fun model -> Pricing.cost ~model ~date usage)
