/* The relay's dealings with the image's own files (see relay_image.h). */

#define _GNU_SOURCE
#include "relay_image.h"

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

/* Elf_probe.prefix_length: test_vm_exec_probe checks that they agree. */
#define EXEC_PREFIX 262144

/* The largest libc.so.6 that is read (glibc's is a few megabytes). */
#define MAX_LIBC (64L * 1024 * 1024)

static const char *const lib_dirs[] = {"/lib", "/lib64", "/usr/lib",
                                       "/usr/lib64"};

/* Whether the regular file at PATH holds the bytes NEEDLE. */
static int file_holds(const char *path, const char *needle, size_t len) {
  int fd = open(path, O_RDONLY | O_CLOEXEC);
  if (fd < 0) return 0;
  struct stat st;
  if (fstat(fd, &st) < 0 || !S_ISREG(st.st_mode) || st.st_size <= 0 ||
      st.st_size > MAX_LIBC) {
    close(fd);
    return 0;
  }
  void *map = mmap(NULL, (size_t)st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
  close(fd);
  if (map == MAP_FAILED) return 0;
  int found = memmem(map, (size_t)st.st_size, needle, len) != NULL;
  munmap(map, (size_t)st.st_size);
  return found;
}

/* The version names of a glibc's libc.so.6 are NUL-separated strings of its
   dynamic string table, and GLIBC_2.34 is among them from glibc 2.34 on. */
static int libc_has_2_34(const char *path) {
  static const char needle[] = "\0GLIBC_2.34";
  return file_holds(path, needle, sizeof needle);
}

/* Whether a libc.so.6 of the image, directly in /lib, /lib64, /usr/lib or
   /usr/lib64 or in one of their multiarch directories (x86_64-linux-gnu),
   defines GLIBC_2.34. */
static int image_glibc_2_34(void) {
  char path[4096];
  for (size_t i = 0; i < sizeof lib_dirs / sizeof lib_dirs[0]; i++) {
    if (snprintf(path, sizeof path, "%s/libc.so.6", lib_dirs[i]) <
            (int)sizeof path &&
        libc_has_2_34(path))
      return 1;
    DIR *d = opendir(lib_dirs[i]);
    if (d == NULL) continue;
    struct dirent *e;
    int found = 0;
    while (!found && (e = readdir(d)) != NULL) {
      size_t n = strlen(e->d_name);
      static const char suffix[] = "-linux-gnu";
      found = n >= sizeof suffix - 1 &&
              strcmp(e->d_name + n - (sizeof suffix - 1), suffix) == 0 &&
              snprintf(path, sizeof path, "%s/%s/libc.so.6", lib_dirs[i],
                       e->d_name) < (int)sizeof path &&
              libc_has_2_34(path);
    }
    closedir(d);
    if (found) return 1;
  }
  return 0;
}

/* NAME as the system would find it: a path with a slash, else a regular
   executable file in a directory of PATH. */
static int find_executable(const char *name, char *out, size_t cap) {
  struct stat st;
  if (strchr(name, '/') != NULL) {
    if (snprintf(out, cap, "%s", name) >= (int)cap) return 0;
    return stat(out, &st) == 0 && S_ISREG(st.st_mode) && access(out, X_OK) == 0;
  }
  const char *path = getenv("PATH");
  if (path == NULL) return 0;
  while (*path != '\0') {
    const char *end = strchr(path, ':');
    size_t n = end != NULL ? (size_t)(end - path) : strlen(path);
    if (n > 0 &&
        snprintf(out, cap, "%.*s/%s", (int)n, path, name) < (int)cap &&
        stat(out, &st) == 0 && S_ISREG(st.st_mode) && access(out, X_OK) == 0)
      return 1;
    if (end == NULL) break;
    path = end + 1;
  }
  return 0;
}

int relay_probe_exec(const char *name) {
  char file[4096];
  static unsigned char prefix[EXEC_PREFIX];
  size_t have = 0;
  int found = find_executable(name, file, sizeof file);
  if (found) {
    int fd = open(file, O_RDONLY | O_CLOEXEC);
    if (fd < 0) {
      found = 0;
    } else {
      ssize_t n;
      while (have < sizeof prefix &&
             (n = read(fd, prefix + have, sizeof prefix - have)) != 0) {
        if (n < 0) {
          if (errno == EINTR) continue;
          found = 0;
          break;
        }
        have += (size_t)n;
      }
      close(fd);
    }
  }
  printf("{\"glibc_2_34\":%s,\"found\":%s,\"prefix\":\"",
         image_glibc_2_34() ? "true" : "false", found ? "true" : "false");
  if (found)
    for (size_t i = 0; i < have; i++) printf("%02x", prefix[i]);
  fputs("\"}\n", stdout);
  return fflush(stdout) == 0 ? 0 : 1;
}

/* Whether LIB is one of the entries of PRELOAD, which the loader separates
   with colons or spaces. */
static int preload_has(const char *preload, const char *lib) {
  size_t n = strlen(lib);
  const char *p = preload;
  while (*p != '\0') {
    size_t len = strcspn(p, ": ");
    if (len == n && strncmp(p, lib, n) == 0) return 1;
    p += len;
    if (*p != '\0') p++;
  }
  return 0;
}

static void faketime_fail(const char *what) {
  fprintf(stderr, "bactrian relay: libfaketime: %s\n", what);
  _exit(126);
}

void relay_apply_faketime(long long epoch, const char *dir, const char *libc) {
  char lib[4096];
  if (snprintf(lib, sizeof lib, "%s/%s/libfaketimeMT.so.1", dir, libc) >=
          (int)sizeof lib ||
      access(lib, R_OK) < 0) {
    fprintf(stderr, "bactrian relay: libfaketime: cannot read %s/%s\n", dir,
            libc);
    _exit(126);
  }
  const char *old = getenv("LD_PRELOAD");
  char preload[8192];
  int n;
  if (old == NULL || *old == '\0')
    n = snprintf(preload, sizeof preload, "%s", lib);
  else if (preload_has(old, lib))
    n = snprintf(preload, sizeof preload, "%s", old);
  else
    n = snprintf(preload, sizeof preload, "%s:%s", lib, old);
  if (n < 0 || (size_t)n >= sizeof preload)
    faketime_fail("LD_PRELOAD is too long");
  struct timespec now;
  if (clock_gettime(CLOCK_REALTIME, &now) < 0) _exit(126);
  long long offset_us =
      epoch * 1000000LL - ((long long)now.tv_sec * 1000000LL + now.tv_nsec / 1000);
  char offset[64];
  long long mag = offset_us < 0 ? -offset_us : offset_us;
  snprintf(offset, sizeof offset, "%c%lld.%06lld", offset_us < 0 ? '-' : '+',
           mag / 1000000LL, mag % 1000000LL);
  if (setenv("LD_PRELOAD", preload, 1) < 0 || setenv("FAKETIME", offset, 1) < 0)
    _exit(126);
  unsetenv("FAKETIME_FMT");
}
