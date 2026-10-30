/* unshare(CLONE_NEWUSER) for the Linux sandbox's agent: the forwarder runs as
   root of bubblewrap's user namespace, so the agent gets a user namespace of
   its own in which it is the user again. Elsewhere it fails with ENOSYS. */

#define _GNU_SOURCE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <errno.h>
#include <string.h>

#ifdef __linux__
#include <sched.h>
#endif

value bactrian_unshare_user(value unit)
{
  CAMLparam1(unit);
#ifdef __linux__
  if (unshare(CLONE_NEWUSER) == 0)
    CAMLreturn(Val_none);
  CAMLreturn(caml_alloc_some(caml_copy_string(strerror(errno))));
#else
  CAMLreturn(caml_alloc_some(caml_copy_string(strerror(ENOSYS))));
#endif
}
