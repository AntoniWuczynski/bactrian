type reason =
  | Macos
  | Faketime_off
  | Library_not_found
  | Library_hidden
  | Setuid_sandbox
  | Invalid_sim_now
  | Static_binary
  | Go_binary
  | Unreadable_executable
  | Http_mode
  | Vm

type t = Enforced | Contract_only of reason

let reason_name = function
  | Macos -> "macos"
  | Faketime_off -> "faketime-off"
  | Library_not_found -> "library-not-found"
  | Library_hidden -> "library-hidden"
  | Setuid_sandbox -> "setuid-sandbox"
  | Invalid_sim_now -> "invalid-sim-now"
  | Static_binary -> "static-binary"
  | Go_binary -> "go-binary"
  | Unreadable_executable -> "unreadable-executable"
  | Http_mode -> "http-mode"
  | Vm -> "vm"

let of_kind : Elf_probe.kind -> t = function
  | Dynamic -> Enforced
  | Static -> Contract_only Static_binary
  | Go -> Contract_only Go_binary
  | Not_elf -> Contract_only Unreadable_executable

let to_clock : t -> Trace.clock = function
  | Enforced -> Enforced
  | Contract_only _ -> Contract_only

let to_json : t -> Yojson.Safe.t = function
  | Enforced -> `Assoc [ ("mode", `String "enforced") ]
  | Contract_only r ->
      `Assoc
        [
          ("mode", `String "contract_only"); ("reason", `String (reason_name r));
        ]
