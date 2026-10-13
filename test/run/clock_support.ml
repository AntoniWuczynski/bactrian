(* What the clock tests share (design §5 step 2). *)

open Bactrian_runtime
open Run_support
module J = Yojson.Safe.Util

let linux = Agent_clock.linux ()

let library =
  Agent_clock.locate ~override:(Sys.getenv_opt Agent_clock.override_variable) ()

(* Far from any real time the tests could run at. *)
let sim_now = "2021-06-01T08:00:00Z"
let sim_epoch = 1622534400.

let usable tool =
  Sys.command (Printf.sprintf "%s --version > /dev/null 2>&1" tool) = 0

let skip_unless condition message =
  if not condition then begin
    print_endline message;
    Alcotest.skip ()
  end

let python_agent =
  {|
import json, os, sys, time

def send(m):
    sys.stdout.write(json.dumps(m) + "\n")
    sys.stdout.flush()

for line in sys.stdin:
    m = json.loads(line)
    if m["type"] == "turn":
        send({"v": 1, "type": "reply", "turn": m["turn"], "content": json.dumps(
            {"now": time.time(), "preload": os.environ.get("LD_PRELOAD", "")})})
    elif m["type"] == "end":
        break
|}

let node_agent =
  {|
const rl = require("readline").createInterface({ input: process.stdin });
rl.on("line", (line) => {
  const m = JSON.parse(line);
  if (m.type === "turn") {
    const content = JSON.stringify({ now: Date.now() / 1000, preload: process.env.LD_PRELOAD || "" });
    process.stdout.write(JSON.stringify({ v: 1, type: "reply", turn: m.turn, content }) + "\n");
  } else if (m.type === "end") {
    rl.close();
  }
});
|}

let agents =
  [
    ("python", "python3", [ "python3"; "-I"; "-c"; python_agent ]);
    ("node", "node", [ "node"; "-e"; node_agent ]);
  ]

let reply (r : Run.result) =
  List.find_map
    (fun l ->
      let j = Yojson.Safe.from_string l in
      match J.(to_string (member "type" j)) with
      | "reply" ->
          Some (Yojson.Safe.from_string J.(to_string (member "content" j)))
      | _ -> None)
    (lines (file r "trace.jsonl"))
  |> function
  | Some j -> j
  | None -> Alcotest.fail "no reply in the trace"

let meta_clock (r : Run.result) =
  Yojson.Safe.to_string
    J.(member "clock" (Yojson.Safe.from_file (file r "meta.json")))

let header_clock (r : Run.result) =
  match lines (file r "trace.jsonl") with
  | first :: _ -> J.(to_string (member "clock" (Yojson.Safe.from_string first)))
  | [] -> Alcotest.fail "empty trace"

let run_agent ?(sandbox = Sandbox.off) ctx argv =
  ok
    (execute ctx
       {
         (config ~agent:argv ()) with
         sim_now;
         timezone = "America/New_York";
         faketime = true;
         sandbox;
       })

(* What the label and the agent's own clock must be on this host. *)
let check_clock (r : Run.result) =
  Alcotest.(check string) "outcome" "passed" (outcome_name r);
  let now = J.(to_float (member "now" (reply r))) in
  let preload = J.(to_string (member "preload" (reply r))) in
  if linux then begin
    Alcotest.(check string) "label" {|{"mode":"enforced"}|} (meta_clock r);
    Alcotest.(check string) "trace header" "enforced" (header_clock r);
    Alcotest.(check bool)
      (Printf.sprintf "the agent's clock (%.0f) reads SIM_NOW (%.0f)" now
         sim_epoch)
      true
      (Float.abs (now -. sim_epoch) < 5.);
    Alcotest.(check bool)
      "the library is preloaded" true
      (contains preload "libfaketimeMT.so.1")
  end
  else begin
    Alcotest.(check string)
      "label" {|{"mode":"contract_only","reason":"macos"}|} (meta_clock r);
    Alcotest.(check string) "trace header" "contract_only" (header_clock r);
    Alcotest.(check string) "nothing is preloaded" "" preload;
    Alcotest.(check bool)
      "the agent sees real time" true
      (Float.abs (now -. Unix.gettimeofday ()) < 60.)
  end
