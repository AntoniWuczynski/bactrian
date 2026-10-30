# 0016: DNS lookups and SIGTERM in the Linux sandbox

Date: 2026-10-30

## Context

Under bubblewrap the only violations recorded were the forwarder's relay failures, so a hostile agent's run looked as quiet as a well-behaved one. A Domain Name System (DNS) lookup is the most common first step of an escape, and inside the network namespace it can only go to loopback, where nothing answered.

Port 53 is below `ip_unprivileged_port_start`, which a new network namespace sets to 1024. bubblewrap's `--dev /dev` mounts a `devpts`, which needs root mapped, so bubblewrap runs the command in a second user namespace nested in the one that owns the network namespace. A capability in that nested namespace does not reach the network, so even `--cap-add CAP_NET_BIND_SERVICE` binding port 53 failed with `EACCES` (checked with bubblewrap 0.9.0 on Ubuntu 24.04).

Separately, the driver ended an agent by sending SIGTERM to bubblewrap's process group. `--new-session` puts the forwarder and the agent in a session of their own, so the signal reached only bubblewrap, whose death killed the sandbox through `--die-with-parent`. An agent under bubblewrap got SIGKILL without the grace period every other agent gets.

## Decision

**DNS.** When bubblewrap is not set-user-ID, it runs the forwarder as root of its user namespace (`--uid 0 --gid 0`) with `CAP_NET_BIND_SERVICE` and `CAP_SETFCAP`, and the forwarder gets `--dns=UID:GID`, bactrian's own IDs. The forwarder binds the User Datagram Protocol (UDP) and TCP port 53 on 127.0.0.1, where a resolver without a `nameserver` line sends its queries, and on 127.0.0.53, systemd-resolved's stub, which Ubuntu's `/etc/resolv.conf` names. It reads each query's question with `Dns_message`, a pure parser in `bactrian.core` that never raises and writes every byte of a label outside letters, digits, `-` and `_` as `\DDD`, writes `{"kind":"dns","transport":"udp","name":"example.com","type":"A"}` to the violation pipe, or the reason when the query cannot be read, and answers `REFUSED`. Then it starts the agent through `bactrian __sandbox-user UID:GID`, a single-threaded step that calls `unshare(CLONE_NEWUSER)`, maps the IDs to the forwarder's root and executes the agent. That is the nested namespace bubblewrap makes itself, moved to after the bind. Mapping the parent's root needs `CAP_SETFCAP` at the time of `unshare` (the kernel's `verify_root_map`), which is why the forwarder holds it. The coverage of such a run is `relay-failures-and-dns`.

The agent ends up as before: its own uid and gid, no capability in any set, `NoNewPrivs`, a working `/dev/pts`, and port 53 already taken. The preflight probe runs through the same path and now also fails when the probe holds a capability, so a host where the drop does not happen is not taken for a sandbox.

**SIGTERM.** bactrian gives the forwarder the read end of a second pipe, named by `BACTRIAN_SANDBOX_SIGNAL_FD` like the violation log's. The driver writes a byte to it where it would send SIGTERM to the group (`Agent_driver.with_agent ?terminate`), and the forwarder sends SIGTERM to the agent's process group, as it does for a SIGTERM it receives. SIGKILL to bubblewrap's group after the grace period stays the last resort. The agent does not inherit the descriptor (it is `/dev/null` in the agent) and cannot open the forwarder's through `/proc`.

## Alternatives

- `--dev-bind` of single devices in place of `--dev /dev`, which avoids the nested namespace and needs only `CAP_NET_BIND_SERVICE`. Rejected: the agent would lose `/dev/pts` and every pseudo-terminal.
- Running the agent as root of the user namespace. Rejected: tools behave differently as root, and the agent's view of file ownership would change.
- A `resolv.conf` of bactrian's bound over the host's, so every lookup reaches the forwarder. Not done: `/etc/resolv.conf` is often a symbolic link, and a mount at it would follow the link out of the sandbox's root.
- For SIGTERM, finding the forwarder's process ID through bubblewrap's `--info-fd` and signalling it. That gives bubblewrap's own process 1, not the forwarder, and with `--as-pid-1` the forwarder would have to reap every orphan.

## Consequences

- A lookup through a resolver at another address, such as a `nameserver` on the local network in a regular `/etc/resolv.conf`, fails with `ENETUNREACH` without reaching the forwarder and is not recorded. The coverage label does not tell the cases apart, so bactrian reads `/etc/resolv.conf` as the sandbox shows it (missing, or under the hidden `/run`, means 127.0.0.1) and, when it names a resolver other than the two the forwarder answers, adds a gap naming it, which makes `complete` false.
- A host may refuse what the DNS path needs while it runs the plain sandbox: an AppArmor profile or a limit on nested user namespaces can stop `unshare` or the ID maps. So when the probe fails through the DNS path, it runs again without it, and if that passes the run goes ahead without DNS (`relay-failures-only`) after a warning that names the first cause.
- bubblewrap leaves a command that root starts every capability of root, which the capability check refuses, so bactrian run as root passes `--cap-drop ALL`. Checked with bubblewrap 0.9.0 on Ubuntu 24.04 as root: every capability set but the bounding set reads zero, `no_new_privs` is set, the masks can be neither remounted nor unmounted, and `mknod` and `chroot` are refused. That closes a hole, since an agent run by root used to hold every capability. Root's agent started through `__sandbox-user` would be root of its own user namespace, with every capability there, so for root the forwarder answers no DNS and the coverage stays `relay-failures-only`. An agent run as root keeps root's file permissions, though: it reads any file root may read that the deny-list does not mask, such as `/etc/shadow`, and writes any file of root's under a writable path. Running bactrian as another user avoids that.
- A set-user-ID bubblewrap refuses `--cap-add` to a user, so there the forwarder answers no DNS and the coverage stays `relay-failures-only`, as it does in the virtual machine.
- The forwarder holds two capabilities, valid only in the sandbox's own user namespace, for the whole run.
- Each lookup is one line in the violation log, so an agent that floods port 53 fills the log's 16 MiB limit, which the violations then report as a gap. Only the first 1000 lines become items of `meta.json` and events of the trace, and a gap counts the rest, which stay in `sandbox.log`.
