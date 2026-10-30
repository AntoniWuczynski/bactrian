open Bactrian_core
open Bactrian_runtime
module E = Elf_fixture
module P = Model_proxy

let write ?(perm = 0o755) path contents =
  Out_channel.with_open_bin path (fun oc -> output_string oc contents);
  Unix.chmod path perm

let with_dir f =
  let dir = Filename.temp_dir ~temp_dir:(Sys.getcwd ()) "doctor" "" in
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> Sys.remove (Filename.concat dir name))
        (Sys.readdir dir);
      Unix.rmdir dir)
    (fun () -> f dir)

let status : Doctor.status Alcotest.testable =
  Alcotest.testable
    (fun fmt s -> Format.pp_print_string fmt (Doctor.status_name s))
    ( = )

let check_status name expected (c : Doctor.check) =
  Alcotest.check status name expected c.status

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

let command ?(argv = [ "agent" ]) () : Experiment.agent =
  Command { argv; turn_timeout = 1.; total_timeout = 1. }

let http url : Experiment.agent =
  Http
    { url; namespace_header = "X-Run"; turn_timeout = 1.; total_timeout = 1. }

let target ?(sandbox = `Off) ?(backend = `Auto) ?image ?(faketime = true)
    ?(faketime_requested = false) ?(deny_read = []) ?(checkers = false)
    ?(funded = []) ~dir agent : Doctor.target =
  {
    agent;
    dir;
    sandbox;
    backend;
    image;
    runtime = None;
    deny_read;
    faketime;
    faketime_requested;
    checkers;
    funded;
  }

(* Keys *)

let statuses checks =
  List.map
    (fun (c : Doctor.check) -> (c.name, Doctor.status_name c.status))
    checks

let pairs = Alcotest.(list (pair string string))

let test_keys_without_experiment () =
  Alcotest.check pairs "set is ok, missing is a warning"
    [
      ("key-openai", "ok");
      ("key-anthropic", "warn");
      ("key-gemini", "warn");
      ("key-openrouter", "warn");
    ]
    (statuses (Doctor.key_checks ~keys:[ P.Openai ] ~funded:None))

let test_keys_with_experiment () =
  let funded = [ (P.Anthropic, "anthropic"); (P.Gemini, "gemini") ] in
  let checks = Doctor.key_checks ~keys:[ P.Gemini ] ~funded:(Some funded) in
  Alcotest.check pairs "a funded provider's missing key fails"
    [
      ("key-openai", "skip");
      ("key-anthropic", "fail");
      ("key-gemini", "ok");
      ("key-openrouter", "skip");
    ]
    (statuses checks);
  Alcotest.(check bool)
    "the message names the variable" true
    (contains ~sub:"ANTHROPIC_API_KEY"
       (List.find (fun (c : Doctor.check) -> c.name = "key-anthropic") checks)
         .message)

(* The agent *)

let test_agent () =
  with_dir @@ fun dir ->
  write (Filename.concat dir "agent") "#!/bin/sh\n";
  write ~perm:0o644 (Filename.concat dir "plain") "";
  let agent argv = Doctor.agent_check ~path:None ~cwd:dir (command ~argv ()) in
  check_status "found relative to the directory" Pass (agent [ "./agent" ]);
  check_status "not executable" Fail (agent [ "./plain" ]);
  check_status "missing" Fail (agent [ "./nothing" ]);
  check_status "no command" Fail (agent []);
  check_status "found on PATH" Pass
    (Doctor.agent_check ~path:(Some dir) ~cwd:"/" (command ()));
  check_status "not on PATH" Fail
    (Doctor.agent_check ~path:(Some "/nonexistent") ~cwd:dir (command ()));
  let url u = Doctor.agent_check ~path:None ~cwd:dir (http u) in
  check_status "http URL" Pass (url "http://127.0.0.1:9/agent");
  check_status "https URL" Pass (url "https://agent.example/run");
  check_status "no host" Fail (url "http:///agent");
  Alcotest.(check string)
    "credentials are not shown"
    "http://h:9/agent is a well-formed URL (not contacted)"
    (url "http://user:secret@h:9/agent?token=t#f").message;
  check_status "no scheme" Fail (url "127.0.0.1:9/agent");
  check_status "another scheme" Fail (url "ftp://agent.example/run")

(* The sandbox *)

let test_sandbox () =
  let calls = ref 0 in
  let probe result () =
    incr calls;
    result
  in
  let run ?(http_agent = false) ?(relay_pending = false) mode result =
    Doctor.sandbox_check ~mode ~http_agent ~relay_pending ~probe:(probe result)
  in
  check_status "works" Pass (run `Strict (Ok ()));
  check_status "unavailable under auto" Warn (run `Auto (Error "no bwrap"));
  check_status "unavailable under strict" Fail (run `Strict (Error "no bwrap"));
  Alcotest.(check int) "probed three times" 3 !calls;
  calls := 0;
  check_status "off" Skip (run `Off (Error "no bwrap"));
  check_status "http agent" Skip (run ~http_agent:true `Strict (Error "x"));
  check_status "a relay not built yet" Skip
    (run ~relay_pending:true `Strict (Error "x"));
  Alcotest.(check int) "none probes" 0 !calls;
  let fix = "Fix it once as root:\n  sudo sysctl -w a=0\n  sudo systemctl x" in
  let c = run `Strict (Error fix) in
  Alcotest.(check bool)
    "the cause and its fix commands are kept" true
    (contains ~sub:"sudo sysctl -w a=0" c.message)

let test_checkers_sandbox () =
  let calls = ref 0 in
  let run mode result =
    Doctor.checkers_sandbox_check ~mode ~probe:(fun () ->
        incr calls;
        result)
  in
  check_status "works" Pass (run `Strict (Ok ()));
  check_status "unavailable under auto" Warn (run `Auto (Error "no bwrap"));
  check_status "unavailable under strict" Fail (run `Strict (Error "no bwrap"));
  Alcotest.(check int) "probed three times" 3 !calls;
  calls := 0;
  check_status "off" Skip (run `Off (Error "no bwrap"));
  Alcotest.(check int) "off does not probe" 0 !calls;
  let c = run `Strict (Error "no bwrap") in
  Alcotest.(check string) "its own name" "sandbox-checkers" c.name;
  Alcotest.(check bool)
    "the cause is kept" true
    (contains ~sub:"no bwrap" c.message)

(* The virtual machine *)

let test_vm () =
  let host ?(macos = true) ?(runtime = Ok "docker (arm64)") ?(image = Ok ())
      ?(cached = Ok true) () : Doctor.vm_host =
    {
      macos;
      detect = (fun () -> runtime);
      image_present = (fun (_ : string) -> image);
      relay_cached = (fun () -> cached);
    }
  in
  let vm_target ?(sandbox = `Strict) ?(backend = `Auto) ?image () =
    target ~sandbox ~backend ?image ~dir:"." (command ())
  in
  let run ?(host = host ()) t = Doctor.vm_check ~host t in
  check_status "works" Pass (run (Some (vm_target ~image:"i" ())));
  let c =
    run ~host:(host ~cached:(Ok false) ()) (Some (vm_target ~image:"i" ()))
  in
  Alcotest.(check bool)
    "the relay is built on first use" true
    (contains ~sub:"first use" c.message);
  check_status "an untrusted relay cache under strict" Fail
    (run
       ~host:(host ~cached:(Error "writable by others") ())
       (Some (vm_target ~image:"i" ())));
  check_status "missing image under strict" Fail
    (run
       ~host:(host ~image:(Error "absent") ())
       (Some (vm_target ~image:"i" ())));
  check_status "missing image under auto" Warn
    (run
       ~host:(host ~image:(Error "absent") ())
       (Some (vm_target ~sandbox:`Auto ~image:"i" ())));
  check_status "no runtime for backend vm" Fail
    (run
       ~host:(host ~runtime:(Error "no docker") ())
       (Some (vm_target ~backend:`Vm ~image:"i" ())));
  let c = run ~host:(host ~runtime:(Error "docker is not on PATH") ()) None in
  check_status "no runtime on macOS" Warn c;
  Alcotest.(check bool)
    "names the runtimes" true
    (contains ~sub:"OrbStack or Docker Desktop" c.message
    && contains ~sub:"Apple's container" c.message);
  check_status "no runtime on Linux" Skip
    (run ~host:(host ~macos:false ~runtime:(Error "no docker") ()) None);
  check_status "no image on macOS: Seatbelt" Warn
    (run (Some (vm_target ~sandbox:`Auto ())));
  check_status "off" Skip (run (Some (vm_target ~sandbox:`Off ())));
  check_status "the host's own sandbox named" Skip
    (run (Some (vm_target ~backend:`Seatbelt ())))

(* The clock *)

let test_clock () =
  with_dir @@ fun dir ->
  let at name = Filename.concat dir name in
  let lib = at "libfaketimeMT.so.1" in
  write lib "";
  write (at "dyn") (E.elf [ (E.pt_interp, E.interp) ]);
  write (at "stat") (E.elf [ (E.pt_load, "code") ]);
  let clock ?(linux = true) ?(faketime = true) ?faketime_requested ?sandbox
      ?(hidden = fun (_ : string) -> false) ?(override = Some lib) agent =
    Doctor.clock_check ~linux ~library_override:override ~path:(Some dir)
      ~hidden
      (Some
         (target ~faketime ?faketime_requested ?sandbox ~dir
            (command ~argv:[ agent ] ())))
  in
  let c = clock "dyn" in
  check_status "enforced" Pass c;
  Alcotest.(check bool)
    "says enforced" true
    (contains ~sub:"enforced" c.message);
  check_status "macOS is expected" Pass (clock ~linux:false "dyn");
  Alcotest.(check bool)
    "with its reason" true
    (contains ~sub:"(macos)" (clock ~linux:false "dyn").message);
  check_status "faketime off" Skip (clock ~faketime:false "dyn");
  check_status "no library" Warn (clock ~override:(Some (at "none")) "dyn");
  check_status "static binary" Warn (clock "stat");
  check_status "agent not found" Warn (clock "absent");
  check_status "http agent" Skip
    (Doctor.clock_check ~linux:true ~library_override:(Some lib) ~path:None
       ~hidden:(fun _ -> false)
       (Some (target ~dir (http "http://127.0.0.1:9/a"))));
  (* Only the sandbox can hide the library, and only when it applies. *)
  let hides_lib p = String.equal p lib in
  let hidden = clock ~sandbox:`Auto ~hidden:hides_lib "dyn" in
  check_status "hidden by deny_read" Warn hidden;
  Alcotest.(check bool)
    "names the reason" true
    (contains ~sub:"library-hidden" hidden.message);
  check_status "hidden under strict too" Warn
    (clock ~sandbox:`Strict ~hidden:hides_lib "dyn");
  check_status "no sandbox, nothing hides it" Pass
    (clock ~sandbox:`Off ~hidden:hides_lib "dyn");
  check_status "another path is not hidden" Pass
    (clock ~sandbox:`Auto ~hidden:(String.equal "/elsewhere") "dyn");
  (* An explicit faketime: auto is a request the host may not meet. *)
  check_status "macOS by default" Pass (clock ~linux:false "dyn");
  let asked = clock ~linux:false ~faketime_requested:true "dyn" in
  check_status "macOS when asked for" Warn asked;
  Alcotest.(check bool)
    "says why" true
    (contains ~sub:"asks for clock: {faketime: auto}" asked.message);
  check_status "enforced when asked for" Pass
    (clock ~faketime_requested:true "dyn");
  check_status "no library when asked for" Warn
    (clock ~faketime_requested:true ~override:(Some (at "none")) "dyn");
  check_status "http agent when asked for" Warn
    (Doctor.clock_check ~linux:true ~library_override:(Some lib) ~path:None
       ~hidden:(fun _ -> false)
       (Some
          (target ~faketime_requested:true ~dir (http "http://127.0.0.1:9/a"))));
  let host ~linux ~override =
    Doctor.clock_check ~linux ~library_override:override ~path:None
      ~hidden:(fun _ -> false)
      None
  in
  check_status "no experiment, macOS" Pass (host ~linux:false ~override:None);
  check_status "no experiment, library found" Pass
    (host ~linux:true ~override:(Some lib));
  check_status "no experiment, library missing" Warn
    (host ~linux:true ~override:(Some (at "none")))

(* The deny-list, asked of settings alone: nothing is made on disk. *)
let test_library_masked () =
  with_dir @@ fun dir ->
  let project = Filename.concat dir "project" in
  let bactrian_dir = Filename.concat project ".bactrian" in
  let masked deny_read library =
    Sandbox.library_masked
      {
        mode = `Auto;
        backend = `Auto;
        image = None;
        runtime = None;
        allow_write = [];
        deny_read;
        bactrian_exe = "bactrian";
        bactrian_dir;
      }
      library
  in
  let lib = "/opt/faketime/libfaketimeMT.so.1" in
  Alcotest.(check bool) "nothing denied" false (masked [] lib);
  Alcotest.(check bool)
    "its directory is denied" true
    (masked [ "/opt/faketime" ] lib);
  Alcotest.(check bool) "the file itself" true (masked [ lib ] lib);
  Alcotest.(check bool)
    "a sibling directory with the same prefix" false
    (masked [ "/opt/fake" ] lib);
  Alcotest.(check bool)
    "under .bactrian/blobs, which is always denied" true
    (masked [] (Filename.concat bactrian_dir "blobs/libfaketimeMT.so.1"));
  Alcotest.(check bool) "no .bactrian made" false (Sys.file_exists bactrian_dir)

(* TLS and egress *)

let test_tls () =
  let tls ?(available = false) ?(upstream = P.default_upstream) t =
    Doctor.tls_check ~available ~upstream t
  in
  let funded = [ (P.Openai, "openai") ] in
  let local _ = Uri.of_string "http://127.0.0.1:11434" in
  check_status "built in" Pass (tls ~available:true None);
  check_status "no experiment" Skip (tls None);
  check_status "no provider called live" Skip
    (tls (Some (target ~dir:"." (command ()))));
  check_status "a provider is called live" Fail
    (tls (Some (target ~funded ~dir:"." (command ()))));
  check_status "a provider is called live at an http upstream" Skip
    (tls ~upstream:local (Some (target ~funded ~dir:"." (command ()))));
  check_status "an https upstream that is not a default" Fail
    (tls
       ~upstream:(fun _ -> Uri.of_string "https://gateway.example/v1")
       (Some (target ~funded ~dir:"." (command ()))));
  check_status "an https agent" Fail
    (tls (Some (target ~dir:"." (http "https://a.example/x"))));
  check_status "an http agent" Skip
    (tls (Some (target ~dir:"." (http "http://127.0.0.1:1/x"))));
  Alcotest.check pairs "egress is not built"
    [ ("ca", "skip"); ("node", "skip") ]
    (statuses Doctor.egress_checks)

(* Upstream overrides *)

let test_upstream_checks () =
  let upstream p =
    match p with
    | P.Openai -> Uri.of_string "http://127.0.0.1:11434"
    | P.Anthropic -> Uri.of_string "https://gateway.example/anthropic"
    | P.Gemini -> Uri.of_string "http://10.0.0.5:8080/gemini"
    | P.Openrouter -> P.default_upstream p
  in
  let checks = Doctor.upstream_checks ~upstream in
  Alcotest.check pairs "one line per overridden provider"
    [
      ("upstream-openai", "ok");
      ("upstream-anthropic", "ok");
      ("upstream-gemini", "warn");
    ]
    (statuses checks);
  Alcotest.(check (list string))
    "the URL is shown"
    [
      "http://127.0.0.1:11434";
      "https://gateway.example/anthropic";
      "http://10.0.0.5:8080/gemini is plain HTTP to another machine: the real \
       API key is sent unencrypted";
    ]
    (List.map (fun (c : Doctor.check) -> c.message) checks);
  Alcotest.(check int)
    "none without an override" 0
    (List.length (Doctor.upstream_checks ~upstream:P.default_upstream))

let test_loopback () = check_status "binds" Pass (Doctor.loopback_check ())

(* Output *)

let test_report () =
  let ok : Doctor.check = { name = "a"; status = Pass; message = "fine" } in
  let bad : Doctor.check =
    { name = "b"; status = Fail; message = "first\nsecond" }
  in
  Alcotest.(check bool) "no failure" true (Doctor.report [ ok ]).ok;
  Alcotest.(check bool)
    "warnings and skips do not fail" true
    (Doctor.report [ { ok with status = Warn }; { ok with status = Skip } ]).ok;
  Alcotest.(check bool) "a failure" false (Doctor.report [ ok; bad ]).ok;
  Alcotest.(check string)
    "text, with a long message indented"
    "ok    a: fine\nfail  b: first\n      second\n"
    (Doctor.render_text (Doctor.report [ ok; bad ]));
  Alcotest.(check string)
    "json"
    {|{"checks":[{"name":"a","status":"ok","message":"fine"},{"name":"b","status":"fail","message":"first\nsecond"}],"ok":false}|}
    (Yojson.Safe.to_string (Doctor.to_json (Doctor.report [ ok; bad ])))

(* No output contains a key's value *)

let sentinels =
  List.map
    (fun (p, _) -> (p, "sk-SENTINEL-" ^ Run.key_variable p ^ "-d34db33f"))
    Run.providers

let test_no_key_value_in_output () =
  with_dir @@ fun dir ->
  Eio_main.run @@ fun env ->
  let report t =
    Doctor.report
      (Doctor.run ~env ~bactrian_exe:"/nonexistent" ~tls_available:false
         ~upstream:P.default_upstream ~keys:sentinels
         ~path:(Some "/usr/bin:/bin") t)
  in
  let funded = Executor.funded [ ("openai", 1.); ("gemini", 1.) ] in
  let reports =
    [
      report (Some (target ~funded ~dir (command ())));
      report (Some (target ~dir (http "http://127.0.0.1:9/agent")));
    ]
  in
  List.iter
    (fun r ->
      List.iter
        (fun render ->
          let out = render r in
          Alcotest.(check bool) "the output has checks" true (out <> "");
          List.iter
            (fun (_, value) ->
              Alcotest.(check bool)
                "no key value, nor a part of it" false
                (contains ~sub:value out
                || contains ~sub:"SENTINEL" out
                || contains ~sub:"d34db33f" out))
            sentinels)
        [
          Doctor.render_text;
          (fun r -> Yojson.Safe.to_string (Doctor.to_json r));
        ])
    reports;
  Alcotest.(check bool)
    "the variable names are printed" true
    (contains ~sub:"OPENAI_API_KEY" (Doctor.render_text (List.hd reports)))

let () =
  Alcotest.run "doctor"
    [
      ( "checks",
        [
          Alcotest.test_case "keys without an experiment" `Quick
            test_keys_without_experiment;
          Alcotest.test_case "keys with an experiment" `Quick
            test_keys_with_experiment;
          Alcotest.test_case "agent" `Quick test_agent;
          Alcotest.test_case "sandbox" `Quick test_sandbox;
          Alcotest.test_case "checkers' sandbox" `Quick test_checkers_sandbox;
          Alcotest.test_case "virtual machine" `Quick test_vm;
          Alcotest.test_case "clock" `Quick test_clock;
          Alcotest.test_case "library masked" `Quick test_library_masked;
          Alcotest.test_case "tls and egress" `Quick test_tls;
          Alcotest.test_case "upstream overrides" `Quick test_upstream_checks;
          Alcotest.test_case "loopback" `Quick test_loopback;
        ] );
      ( "output",
        [
          Alcotest.test_case "report" `Quick test_report;
          Alcotest.test_case "no key value is printed" `Quick
            test_no_key_value_in_output;
        ] );
    ]
