/* prctl(PR_SET_DUMPABLE, 0) for the Linux sandbox forwarder: the files under
   /proc/PID of a process that is not dumpable belong to root, so the agent it
   starts cannot reopen its descriptors through /proc/PID/fd. Elsewhere a no-op
   that reports success. */

#include <caml/mlvalues.h>

#ifdef __linux__
#include <sys/prctl.h>
#endif

value bactrian_set_undumpable(value unit)
{
  (void)unit;
#ifdef __linux__
  return Val_bool(prctl(PR_SET_DUMPABLE, 0, 0, 0, 0) == 0);
#else
  return Val_true;
#endif
}
