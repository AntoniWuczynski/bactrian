/* The relay's probe (included by relay.c): the sandbox checks that run in place
   of an agent. Its helpers are the relay's own (json_string, greeting). */

static const char *errno_name(int e) {
  switch (e) {
    case EPERM: return "EPERM";
    case EACCES: return "EACCES";
    case EROFS: return "EROFS";
    case ENOENT: return "ENOENT";
    case EEXIST: return "EEXIST";
    case ENETUNREACH: return "ENETUNREACH";
    case EHOSTUNREACH: return "EHOSTUNREACH";
    case ENETDOWN: return "ENETDOWN";
    case ECONNREFUSED: return "ECONNREFUSED";
    case ETIMEDOUT: return "ETIMEDOUT";
    case EADDRNOTAVAIL: return "EADDRNOTAVAIL";
    default: return "error";
  }
}

/* Connects without sending anything. NULL on success, else the error. */
static const char *probe_connect(const char *ip, int port, int *fd_out) {
  int fd = socket(AF_INET, SOCK_STREAM | SOCK_CLOEXEC | SOCK_NONBLOCK, 0);
  if (fd < 0) return errno_name(errno);
  struct sockaddr_in a;
  memset(&a, 0, sizeof a);
  a.sin_family = AF_INET;
  a.sin_port = htons((uint16_t)port);
  if (inet_pton(AF_INET, ip, &a.sin_addr) != 1) {
    close(fd);
    return "error";
  }
  const char *res = NULL;
  if (connect(fd, (struct sockaddr *)&a, sizeof a) < 0) {
    if (errno != EINPROGRESS) {
      res = errno_name(errno);
    } else {
      struct pollfd p = {fd, POLLOUT, 0};
      int r = poll(&p, 1, 5000);
      if (r == 0) {
        res = "timeout";
      } else if (r < 0) {
        res = errno_name(errno);
      } else {
        int err = 0;
        socklen_t len = sizeof err;
        if (getsockopt(fd, SOL_SOCKET, SO_ERROR, &err, &len) < 0)
          res = errno_name(errno);
        else if (err != 0)
          res = errno_name(err);
      }
    }
  }
  if (res != NULL || fd_out == NULL)
    close(fd);
  else
    *fd_out = fd;
  return res;
}

static const char *probe_listener(int port) {
  int fd = -1;
  const char *res = probe_connect("127.0.0.1", port, &fd);
  if (res != NULL) return res;
  char got[sizeof greeting];
  size_t have = 0, want = sizeof greeting - 1;
  while (have < want) {
    struct pollfd p = {fd, POLLIN, 0};
    int r = poll(&p, 1, 5000);
    if (r == 0) {
      res = "timeout";
      break;
    }
    if (r < 0) {
      res = errno_name(errno);
      break;
    }
    ssize_t n = read(fd, got + have, want - have);
    if (n == 0) {
      res = "closed before the greeting";
      break;
    }
    if (n < 0) {
      if (errno == EAGAIN || errno == EINTR) continue;
      res = errno_name(errno);
      break;
    }
    have += (size_t)n;
  }
  close(fd);
  if (res == NULL && memcmp(got, greeting, want) != 0) res = "unexpected reply";
  return res;
}

static const char *probe_create(const char *path) {
  int fd = open(path, O_WRONLY | O_CREAT | O_EXCL | O_CLOEXEC, 0600);
  if (fd < 0) return errno_name(errno);
  close(fd);
  return NULL;
}

/* NULL when /proc/self/status says NoNewPrivs is 1. */
static const char *probe_no_new_privs(void) {
  FILE *s = fopen("/proc/self/status", "re");
  if (s == NULL) return errno_name(errno);
  char line[256];
  const char *res = "off";
  while (fgets(line, sizeof line, s) != NULL) {
    if (strncmp(line, "NoNewPrivs:", 11) == 0) {
      if (strtol(line + 11, NULL, 10) == 1) res = NULL;
      break;
    }
  }
  fclose(s);
  return res;
}

static const char *probe_read(const char *path) {
  int fd = open(path, O_RDONLY | O_CLOEXEC);
  if (fd < 0) return errno_name(errno);
  char c;
  ssize_t n = read(fd, &c, 1);
  int e = errno;
  close(fd);
  if (n < 0) return errno_name(e);
  return n == 0 ? "empty" : NULL;
}

/* How long the probe waits for the heartbeat file to change. The bridge
   touches it every 2 seconds, so a runtime that shows the change within 3
   seconds of the touch passes, and the relay's timeout (10 seconds) has room
   to spare. */
#ifndef PROBE_BEAT_MS
#define PROBE_BEAT_MS 5000
#endif

/* NULL when no heartbeat file is watched or its modification time changes
   within [wait_ms]: the touches of the host reach the container. Otherwise
   "unchanged", or the error of looking at it. */
static const char *probe_heartbeat(long wait_ms) {
  struct stat st;
  if (heartbeat_path == NULL) return NULL;
  if (stat(heartbeat_path, &st) < 0) return errno_name(errno);
  struct timespec first = st.st_mtim;
  for (long waited = 0; waited < wait_ms; waited += 100) {
    usleep(100000);
    if (stat(heartbeat_path, &st) < 0) return errno_name(errno);
    if (!same_time(&st.st_mtim, &first)) return NULL;
  }
  return "unchanged";
}

/* NULL when a write to the heartbeat file succeeds, which it must not: the
   agent could then keep its own relay alive. Otherwise the error, which should
   be "EROFS", or "unchecked" when no heartbeat file is watched. */
static const char *probe_heartbeat_write(void) {
  if (heartbeat_path == NULL) return "unchecked";
  int fd = open(heartbeat_path, O_WRONLY | O_CLOEXEC);
  if (fd < 0) return errno_name(errno);
  close(fd);
  return NULL;
}

static void put_outcome(FILE *f, const char *name, const char *res, int comma) {
  fprintf(f, "\"%s\":\"%s\"%s", name, res == NULL ? "ok" : res,
          comma ? "," : "");
}

/* paths: OUTSIDE INSIDE DENIED MASKED MASKED_CASE ROOT OTHER_RUN */
static int run_probe(int port, const char **paths) {
  const char *public_ = probe_connect("1.1.1.1", 443, NULL);
  const char *listener = probe_listener(port);
  const char *outside_w = probe_create(paths[0]);
  const char *inside_w = probe_create(paths[1]);
  const char *denied_r = probe_read(paths[2]);
  const char *masked_r = probe_read(paths[3]);
  const char *masked_case_r = probe_read(paths[4]);
  const char *root_w = probe_create(paths[5]);
  const char *other_run_w = probe_create(paths[6]);
  const char *no_new_privs = probe_no_new_privs();
  const char *heartbeat = probe_heartbeat(PROBE_BEAT_MS);
  const char *heartbeat_w = probe_heartbeat_write();
  FILE *f = stdout;
  fputc('{', f);
  put_outcome(f, "public", public_, 1);
  put_outcome(f, "listener", listener, 1);
  put_outcome(f, "outside_write", outside_w, 1);
  put_outcome(f, "inside_write", inside_w, 1);
  put_outcome(f, "denied_read", denied_r, 1);
  put_outcome(f, "masked_read", masked_r, 1);
  put_outcome(f, "masked_case_read", masked_case_r, 1);
  put_outcome(f, "root_write", root_w, 1);
  put_outcome(f, "other_run_write", other_run_w, 1);
  put_outcome(f, "no_new_privs", no_new_privs, 1);
  put_outcome(f, "heartbeat", heartbeat, 1);
  put_outcome(f, "heartbeat_write", heartbeat_w, 1);
  DIR *d = opendir("/sys/class/net");
  put_outcome(f, "interfaces_listed", d == NULL ? errno_name(errno) : NULL, 1);
  fputs("\"interfaces\":[", f);
  int first = 1;
  if (d != NULL) {
    struct dirent *e;
    while ((e = readdir(d)) != NULL) {
      if (e->d_name[0] == '.') continue;
      char esc[256];
      json_string(esc, sizeof esc, e->d_name);
      fprintf(f, "%s\"%s\"", first ? "" : ",", esc);
      first = 0;
    }
    closedir(d);
  }
  fputs("]}\n", f);
  return fflush(f) == 0 ? 0 : 1;
}
