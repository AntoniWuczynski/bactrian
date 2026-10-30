# 0017: bactrian's own HTTPS calls and the roots it trusts

Date: 2026-10-30

## Context

bactrian calls providers, `https` upstreams (decision 0012) and HTTP-mode agents at `https` URLs (decision 0008) itself, through the model proxy's upstream client. That client has its own HTTP/1.1 reader, because a stream cut off inside a chunked body has to be told apart from a finished one. Until now the TLS connector it was designed to take was not built, and every such call stopped with exit status 69.

The obvious way to find the system's root certificates in OCaml is the `ca-certs` library (1.0.3). Its source (`lib/ca_certs.ml`) shows two problems for bactrian:

- It reads `SSL_CERT_FILE`, then `NIX_SSL_CERT_FILE`, before looking at the system, and replaces the system's roots with that file. It also adds `OCAML_EXTRA_CA_CERTS`. `SSL_CERT_FILE` is exactly the variable that egress interception will point at bactrian's own certificate authority for agents, and a user may have it set for other tools.
- It runs `uname -s`, and on macOS `security find-certificate`, through `Bos`, which starts them with the calling process's environment. After the restore of decision 0010 that environment holds the real provider keys, and decision 0010 says that no child inherits it.

What `tls-eio` 2.1.4 does matters for the reader (`eio/tls_eio.ml`, `lib/config.mli`). A TCP close without a TLS close_notify reads as end of file, like a clean close. TLS errors are raised as `Tls_eio.Tls_failure` and `Tls_eio.Tls_alert`, not as `Eio.Io`. `client_of_flow` checks a host name only when given one (`?host`) or an address (`?ip`).

## Decision

A library of its own, `bactrian.tls`, holds the TLS stack (`tls-eio` and `x509`), so `bactrian.runtime` still builds without it and takes the connector as a value.

**Trust.** `Trust` finds the same system roots as `ca-certs`: on macOS the `SystemRootCertificates` and `System` keychains, read with `/usr/bin/security`, on Linux the first bundle found of `/etc/ssl/certs/ca-certificates.crt`, `/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem`, `/etc/ssl/ca-bundle.pem` and `/etc/ssl/cert.pem`. `security` is started through Eio with an empty environment. `SSL_CERT_FILE`, `NIX_SSL_CERT_FILE` and `OCAML_EXTRA_CA_CERTS` are never read. `BACTRIAN_EXTRA_CA_CERTS` names a file of PEM certificates that are trusted as well as the system's, never instead of them, for a proxy that inspects TLS and for tests. A `BACTRIAN_*` variable survives the launch-environment scrub (decision 0012), and its name says it is bactrian's own. The roots load once per command, on the first call that needs them.

**Connections.** `Tls_connector` verifies the chain against those roots at the current time, sends the URL's host as Server Name Indication and checks it against the certificate, as an IP address when the host parses as one and as a DNS name otherwise. A host that is neither is refused before the handshake, so no connection is made without a name check. ALPN offers only `http/1.1`, so every response is read by bactrian's own reader, which needs the content length or the terminal chunk. A stream cut off by a TCP close, with or without close_notify, is therefore `Truncated` in both cases. TLS 1.2 and 1.3 are both allowed (the library's default range).

**Errors.** The connector's `https` record in `Upstream` has three parts. `ready` loads the roots and says why HTTPS cannot work, `wrap` does the handshake and returns the reason it failed, and `failure` names the TLS exceptions a wrapped flow raises mid-body, so the call ends with `Truncated` and not an exception. Only `https` URLs are wrapped.

**Refusals.** `Live_preflight` calls `ready` only when a provider with a spend cap has an `https` upstream, and the executor only for an `https` HTTP-mode agent. When `ready` fails, the command stops with exit status 69 and the reason, as the old "no TLS support" refusal did. A certificate that does not verify, or a host name that does not match, is a failed call, recorded as the transport error it is (or `agent_http_error` for an HTTP-mode agent). `doctor`'s `tls` check calls `ready`. It is `ok` when the roots load, and otherwise a failure for an experiment that needs HTTPS and a warning for one that does not.

## Consequences

- The egress proxy no longer needs to scrub `SSL_CERT_FILE` from bactrian's environment to keep its upstream trust: the trust bundle it will give agents cannot reach bactrian's own.
- A user who relies on `SSL_CERT_FILE` for a proxy that inspects TLS has to set `BACTRIAN_EXTRA_CA_CERTS` as well. The README says so.
- HTTP/2-only servers cannot be reached. No provider is known to need HTTP/2 (research for this project found no source saying so), and HTTP/1.1 keeps streaming on the reader that detects truncation.
- The tests mint a certificate authority and leaf certificates in memory for each test (`test/support/fake_https`), so no private key is ever committed, and they run without a network. Calls to the real providers over HTTPS are not tested here.
- Other BSDs and Windows are not handled, as bactrian supports only macOS and Linux.
