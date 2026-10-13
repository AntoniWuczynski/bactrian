open Bactrian_runtime

let env =
  [|
    "PATH=/usr/bin:/bin";
    "HOME=/home/u";
    "OPENAI_API_KEY=sk-real";
    "LC_ALL=C.UTF-8";
    "LCX=no";
    "TERM=xterm";
    "BACTRIAN_CAP_OPENAI=5";
    "TZ=UTC";
    "USER=u";
    "LOGNAME=u";
    "SHELL=/bin/zsh";
    "LANG=en_GB.UTF-8";
    "TMPDIR=/tmp/u";
    "no-equals-sign";
    "AWS_SECRET_ACCESS_KEY=a=b";
  |]

let test_kept () =
  Alcotest.(check (list string))
    "only the allowlist stays, in order"
    [
      "PATH=/usr/bin:/bin";
      "HOME=/home/u";
      "LC_ALL=C.UTF-8";
      "TERM=xterm";
      "TZ=UTC";
      "USER=u";
      "LOGNAME=u";
      "SHELL=/bin/zsh";
      "LANG=en_GB.UTF-8";
      "TMPDIR=/tmp/u";
    ]
    (Launch_env.kept env);
  List.iter
    (fun name -> Alcotest.(check bool) name false (Launch_env.allowed name))
    [ "OPENAI_API_KEY"; Launch_env.marker; "LCX"; "lc_all"; "PATHX"; "" ]

let test_round_trip () =
  Alcotest.(check (list (pair string string)))
    "every entry with a name, split at the first ="
    [
      ("PATH", "/usr/bin:/bin");
      ("EMPTY", "");
      ("AWS_SECRET_ACCESS_KEY", "a=b");
      ("MULTI", "line\nbreak");
    ]
    (Launch_env.decode
       (Launch_env.encode
          [|
            "PATH=/usr/bin:/bin";
            "EMPTY=";
            "=nameless";
            "AWS_SECRET_ACCESS_KEY=a=b";
            "no-equals-sign";
            "MULTI=line\nbreak";
          |]));
  Alcotest.(check (list (pair string string)))
    "nothing" []
    (Launch_env.decode (Launch_env.encode [||]))

let () =
  Alcotest.run "launch_env"
    [
      ( "launch_env",
        [
          Alcotest.test_case "allowlist split" `Quick test_kept;
          Alcotest.test_case "encode and decode" `Quick test_round_trip;
        ] );
    ]
