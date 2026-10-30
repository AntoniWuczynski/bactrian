The project CA is made on first use in .bactrian/ca: the directory and the key
are private to the user, the certificate is readable.

  $ bactrian ca path
  $TESTCASE_ROOT/.bactrian/ca/ca.pem
  $ for f in .bactrian/ca .bactrian/ca/ca.key .bactrian/ca/ca.pem; do ls -ld "$f" | cut -c1-10; done
  drwx------
  -rw-------
  -rw-r--r--
  $ head -1 .bactrian/ca/ca.pem
  -----BEGIN CERTIFICATE-----

The second time it is loaded, not made again. The JSON says so.

  $ bactrian ca path --json | sed -e 's/"not_after":"[^"]*"/"not_after":"T"/' -e 's/"sha256":"[0-9a-f]*"/"sha256":"H"/'
  {"certificate":"$TESTCASE_ROOT/.bactrian/ca/ca.pem","not_after":"T","sha256":"H","created":false}
  $ bactrian ca path --json > before.json

Rotation makes a new one.

  $ bactrian ca rotate | sed -e 's/valid until .*/valid until T/'
  rotated: $TESTCASE_ROOT/.bactrian/ca/ca.pem, valid until T
  $ bactrian ca path --json > after.json
  $ cmp -s before.json after.json || echo changed
  changed

A key that others can read is refused, with the fix.

  $ chmod 644 .bactrian/ca/ca.key
  $ bactrian ca path
  bactrian: the CA key ./.bactrian/ca/ca.key can be used by other users (mode 644): run chmod go= on it
  [69]
