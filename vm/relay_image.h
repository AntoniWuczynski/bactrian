/* What the relay does with the image's own files: the executable probe and
   the agent's clock. Part of the relay (vm/relay.c), built with it. */

#ifndef RELAY_IMAGE_H
#define RELAY_IMAGE_H

/* Writes one JSON line on standard output about NAME in the image and returns
   the process's exit status:
     {"glibc_2_34":BOOL,"found":BOOL,"prefix":"HEX"}
   glibc_2_34 is whether a libc.so.6 of the image defines the symbol version
   GLIBC_2.34, which libfaketime for glibc needs. found is whether NAME is an
   executable file found as the system would (a name with a slash as a path
   from the working directory, else through PATH), and prefix its first
   EXEC_PREFIX bytes in hexadecimal. */
int relay_probe_exec(const char *name);

/* In the agent's process before it starts: DIR/LIBC/libfaketimeMT.so.1
   (LIBC is "musl" or "glibc") in LD_PRELOAD, first and once, and FAKETIME as
   the offset that makes the container's clock read EPOCH now. Exits with 126
   rather than run an agent whose clock was promised and is not there. */
void relay_apply_faketime(long long epoch, const char *dir, const char *libc);

#endif
