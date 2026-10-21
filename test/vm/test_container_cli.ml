(* The JSON of Apple's container 1.5.0, as it printed it (shortened where
   fields bactrian does not read run on). *)

open Bactrian_core

let digest =
  "sha256:294b683cb724975bec92580e1e685676bd4b50bda910ddb8c51d4cabeaec77e6"

let status_running =
  {|{"client":{"appName":"container","build":"release","commit":"d265d669ecae041bf338cb3b39c4118316d138f0","version":"1.5.0"},"host":{"architecture":"arm64","cpus":16,"operatingSystem":"Version 26.6.1 (Build 25G76)"},"paths":{"appRoot":"/Library/Application Support/com.apple.container/","installRoot":"/usr/local/"},"resources":{"containersRunning":0,"containersTotal":0,"images":2},"server":{"appName":"container-apiserver","build":"release","commit":"d265d669ecae041bf338cb3b39c4118316d138f0","version":"1.5.0"},"status":"running"}|}

let image_inspect =
  Printf.sprintf
    {|[
  {
    "configuration" : {
      "creationDate" : "2026-09-17T20:36:41Z",
      "descriptor" : {
        "digest" : "%s",
        "mediaType" : "application/vnd.oci.image.index.v1+json",
        "size" : 9218
      },
      "name" : "docker.io/library/alpine@%s"
    },
    "id" : "294b683cb724975bec92580e1e685676bd4b50bda910ddb8c51d4cabeaec77e6",
    "variants" : [ { "config" : { "architecture" : "amd64" }, "size" : 3851371 } ]
  }
]|}
    digest digest

let container_inspect =
  Printf.sprintf
    {|[{"configuration":{"capAdd":[],"capDrop":[],"creationDate":"2026-10-21T17:09:40Z","id":"bactrian-r1","image":{"descriptor":{"digest":"%s","mediaType":"application/vnd.oci.image.index.v1+json","size":9218},"reference":"docker.io/library/alpine@%s"},"initProcess":{"arguments":["-c","exit 0"],"executable":"/bin/sh","terminal":false,"user":{"id":{"gid":0,"uid":0}},"workingDirectory":"/"},"labels":{"bactrian.host":"h","bactrian.pid":"1"},"mounts":[],"networks":[],"readOnly":false,"resources":{"cpus":4,"memoryInBytes":1073741824}},"id":"bactrian-r1","status":{"networks":[],"state":"stopped"}}]|}
    digest digest

let listing =
  {|[{"configuration":{"id":"bactrian-r1","image":{"descriptor":{"digest":"sha256:00"},"reference":"r"},"labels":{"bactrian.host":"h","bactrian.pid":"1"}},"id":"bactrian-r1","status":{"state":"stopped"}},{"configuration":{"id":"other","labels":{}},"id":"other","status":{"state":"running"}}]|}

let result_of = Alcotest.(result string string)

let test_status () =
  (match Container_cli.status status_running with
  | Ok s ->
      Alcotest.(check bool) "running" true s.running;
      Alcotest.(check string) "architecture" "arm64" s.architecture
  | Error m -> Alcotest.failf "status: %s" m);
  (match
     Container_cli.status
       {|{"status":"stopped","host":{"architecture":"arm64"}}|}
   with
  | Ok s ->
      Alcotest.(check bool) "stopped" false s.running;
      Alcotest.(check string) "its state" "stopped" s.state
  | Error m -> Alcotest.failf "status: %s" m);
  Alcotest.(check bool)
    "no host" true
    (Result.is_error (Container_cli.status {|{"status":"running"}|}));
  Alcotest.(check bool)
    "not JSON" true
    (Result.is_error (Container_cli.status "Error: XPC connection error"))

let test_digests () =
  Alcotest.check result_of "image inspect" (Ok digest)
    (Container_cli.image_digest image_inspect);
  Alcotest.check result_of "container inspect" (Ok digest)
    (Container_cli.container_digest container_inspect);
  Alcotest.check result_of "an empty inspect"
    (Error "image inspect gave an empty list")
    (Container_cli.image_digest "[]");
  Alcotest.(check bool)
    "a digest of another type" true
    (Result.is_error
       (Container_cli.container_digest
          {|[{"configuration":{"image":{"descriptor":{"digest":1}}}}]|}))

let test_listing () =
  match Container_cli.containers listing with
  | Ok cs ->
      Alcotest.(check (list (pair string (list (pair string string)))))
        "names and labels"
        [
          ("bactrian-r1", [ ("bactrian.host", "h"); ("bactrian.pid", "1") ]);
          ("other", []);
        ]
        (List.map (fun (c : Container_cli.listed) -> (c.id, c.labels)) cs)
  | Error m -> Alcotest.failf "list: %s" m

let test_bad_entries () =
  let ids text =
    Result.map
      (List.map (fun (c : Container_cli.listed) -> c.id))
      (Container_cli.containers text)
  in
  Alcotest.(check (result (list string) string))
    "only the entries that decode" (Ok [ "bactrian-r1" ])
    (ids
       {|[{"configuration":{"id":"x","labels":{"bactrian.pid":1}}},{"configuration":{"labels":{}}},{"configuration":{"id":"bactrian-r1","labels":{}}}]|});
  Alcotest.(check bool)
    "not a list" true
    (Result.is_error (Container_cli.containers {|{"configuration":{}}|}))

let () =
  Alcotest.run "container_cli"
    [
      ( "decode",
        [
          Alcotest.test_case "system status" `Quick test_status;
          Alcotest.test_case "digests" `Quick test_digests;
          Alcotest.test_case "list" `Quick test_listing;
          Alcotest.test_case "entries that do not decode" `Quick
            test_bad_entries;
        ] );
    ]
