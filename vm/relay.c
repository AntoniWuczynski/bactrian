/* The entrypoint of a bactrian virtual machine sandbox (design section 5.3).

   The container has no network (--network none). Its standard input and
   output are the one channel to bactrian, and carry frames of
     [channel u32 BE][kind u8][length u32 BE][payload]
   Channel 0 is the agent's standard input (bactrian to agent) and standard
   output (agent to bactrian). For each --forward PORT the relay listens on
   127.0.0.1:PORT, and every accepted connection gets a new odd channel:
   an open frame (payload: the port, u16 BE), then data and eof both ways,
   then close. The last frame the relay writes is exit (payload: the agent's
   status, u32 BE, 128 plus the signal number when a signal ended it). Log
   frames carry one JSON line each, in the shape of the Linux forwarder's
   relay failures. bactrian sends a heartbeat frame on channel 0 every two
   seconds. The agent's standard error is the container's, unframed.

   The relay is PID 1: it reaps every child, forwards SIGTERM and SIGINT to
   the agent's process group, and exits with the agent's status. When nothing
   has come from bactrian for LIVENESS_MS while the relay was reading, it
   takes bactrian for dead (the runtime's client can outlive it and keep the
   input open), kills the agent's group and exits. It makes
   itself not dumpable, so the agent (the same user) can neither trace it nor
   open its descriptors through /proc/1/fd and write frames of its own. Memory
   is bounded: a destination that does not drain stops the reading of its
   source.

   relay --forward PORT... -- AGENT ARGV...
   relay --forward PORT --probe OUTSIDE INSIDE DENIED MASKED MASKED_CASE ROOT
         OTHER_RUN

   The probe form runs the sandbox checks instead of an agent and writes one
   JSON line on channel 0: the outcome of a connection to a public address and
   to bactrian's listener, of creating OUTSIDE, INSIDE, ROOT and OTHER_RUN, of
   reading DENIED, MASKED and MASKED_CASE, and the network interfaces. */

#define _GNU_SOURCE
#include <arpa/inet.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <poll.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

enum kind {
  K_DATA = 0,
  K_EOF = 1,
  K_OPEN = 2,
  K_CLOSE = 3,
  K_EXIT = 4,
  K_LOG = 5,
  K_HEARTBEAT = 6
};

#define HEADER 9
#define MAX_PAYLOAD 65536
#define CHUNK 16384
#define OUT_CAP (1024 * 1024)
/* Room in out kept for control frames (open, eof, close, exit, log), which
   are queued without a check: data is read only while more than this is free.
*/
#define RESERVE (64 * 1024)
#define SINK_CAP (256 * 1024)
#define MAX_FORWARDS 16
#define MAX_CONNS 64
#define DRAIN_MS 2000
#define EXIT_PROTOCOL 70
/* Five heartbeats (Vm_frame.heartbeat_interval, 2 s) missed. */
#define LIVENESS_MS 10000

static const char greeting[] = "bactrian-probe\n";

/* A bounded byte queue: bytes are appended at [len] and taken from [off]. */
struct buf {
  unsigned char *data;
  size_t cap, off, len;
};

__attribute__((noreturn)) static void die(const char *what) {
  fprintf(stderr, "bactrian relay: %s: %s\n", what, strerror(errno));
  exit(EXIT_PROTOCOL);
}

static void buf_init(struct buf *b, size_t cap) {
  b->data = malloc(cap);
  if (b->data == NULL) die("malloc");
  b->cap = cap;
  b->off = 0;
  b->len = 0;
}

static size_t buf_used(const struct buf *b) { return b->len - b->off; }

static size_t buf_room(const struct buf *b) { return b->cap - buf_used(b); }

/* The caller checks buf_room first. */
static void buf_put(struct buf *b, const void *p, size_t n) {
  if (b->cap - b->len < n) {
    memmove(b->data, b->data + b->off, buf_used(b));
    b->len -= b->off;
    b->off = 0;
  }
  memcpy(b->data + b->len, p, n);
  b->len += n;
}

static void buf_take(struct buf *b, size_t n) {
  b->off += n;
  if (b->off == b->len) b->off = b->len = 0;
}

static void put_u32(unsigned char *p, uint32_t v) {
  p[0] = (unsigned char)(v >> 24);
  p[1] = (unsigned char)(v >> 16);
  p[2] = (unsigned char)(v >> 8);
  p[3] = (unsigned char)v;
}

static uint32_t get_u32(const unsigned char *p) {
  return ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) |
         ((uint32_t)p[2] << 8) | (uint32_t)p[3];
}

/* State */

struct conn {
  int fd; /* -1 when the slot is free */
  uint32_t channel;
  int port;
  struct buf sink;  /* bytes from bactrian to the connection */
  int eof_in;       /* bactrian sent eof: shut down writing once drained */
  int shut;         /* writing is shut down */
  int eof_out;      /* the connection reached end of file, eof was sent */
};

static struct buf out;   /* frames to bactrian */
static struct buf to_agent;
static int agent_in = -1, agent_out = -1;
static int agent_in_eof; /* bactrian closed the agent's standard input */
static pid_t agent_pid = -1;
static int agent_status = -1; /* the exit code, once the agent is reaped */
static struct conn conns[MAX_CONNS];
static int listeners[MAX_FORWARDS], ports[MAX_FORWARDS], nforwards;
static uint32_t next_channel = 1;
static int sig_pipe[2];
static volatile sig_atomic_t got_term, got_int;

/* Frames are queued whole. Callers check that out has room for one frame of
   their payload's size before they read from a source. */
static void frame(uint32_t channel, enum kind k, const void *payload,
                  size_t n) {
  unsigned char h[HEADER];
  put_u32(h, channel);
  h[4] = (unsigned char)k;
  put_u32(h + 5, (uint32_t)n);
  buf_put(&out, h, HEADER);
  if (n > 0) buf_put(&out, payload, n);
}

/* Whether a data frame of n bytes may be queued. */
static int out_has_room(size_t n) {
  return buf_room(&out) >= HEADER + n + RESERVE;
}

static void json_string(char *dst, size_t cap, const char *s) {
  size_t j = 0;
  for (; *s != '\0' && j + 7 < cap; s++) {
    unsigned char c = (unsigned char)*s;
    if (c == '"' || c == '\\') {
      dst[j++] = '\\';
      dst[j++] = (char)c;
    } else if (c < 0x20 || c > 0x7e) {
      j += (size_t)snprintf(dst + j, cap - j, "\\u%04x", c);
    } else {
      dst[j++] = (char)c;
    }
  }
  dst[j] = '\0';
}

/* A relay failure, as the Linux forwarder logs it. Dropped when out is full:
   the log is a lower bound, never a reason to block. */
static void log_failure(int port, const char *where, const char *reason) {
  char esc[512], line[700];
  json_string(esc, sizeof esc, reason);
  int n = snprintf(line, sizeof line,
                   "{\"kind\":\"relay_failed\",\"port\":%d,\"socket\":\"%s\","
                   "\"reason\":\"%s\"}",
                   port, where, esc);
  if (n > 0 && (size_t)n < sizeof line && buf_room(&out) >= HEADER + (size_t)n)
    frame(0, K_LOG, line, (size_t)n);
}

static void set_nonblock(int fd) {
  int flags = fcntl(fd, F_GETFL);
  if (flags < 0 || fcntl(fd, F_SETFL, flags | O_NONBLOCK) < 0)
    die("fcntl");
}

static void close_fd(int *fd) {
  if (*fd >= 0) {
    while (close(*fd) < 0 && errno == EINTR) {
    }
    *fd = -1;
  }
}

/* Signals */

static void on_signal(int s) {
  int saved = errno;
  if (s == SIGTERM) got_term = 1;
  if (s == SIGINT) got_int = 1;
  char c = (char)s;
  /* A full pipe already holds a wake-up. */
  if (write(sig_pipe[1], &c, 1) < 0) {
  }
  errno = saved;
}

static void install_signals(void) {
  if (pipe(sig_pipe) < 0) die("pipe");
  set_nonblock(sig_pipe[0]);
  set_nonblock(sig_pipe[1]);
  if (fcntl(sig_pipe[0], F_SETFD, FD_CLOEXEC) < 0 ||
      fcntl(sig_pipe[1], F_SETFD, FD_CLOEXEC) < 0)
    die("fcntl");
  struct sigaction sa;
  memset(&sa, 0, sizeof sa);
  sa.sa_handler = on_signal;
  sa.sa_flags = SA_RESTART | SA_NOCLDSTOP;
  if (sigemptyset(&sa.sa_mask) < 0) die("sigemptyset");
  if (sigaction(SIGCHLD, &sa, NULL) < 0 || sigaction(SIGTERM, &sa, NULL) < 0 ||
      sigaction(SIGINT, &sa, NULL) < 0)
    die("sigaction");
  struct sigaction ign;
  memset(&ign, 0, sizeof ign);
  ign.sa_handler = SIG_IGN;
  if (sigemptyset(&ign.sa_mask) < 0 || sigaction(SIGPIPE, &ign, NULL) < 0)
    die("sigaction");
}

static void signal_agent(int s) {
  if (agent_pid > 0 && kill(-agent_pid, s) < 0 && errno != ESRCH)
    fprintf(stderr, "bactrian relay: kill: %s\n", strerror(errno));
}

/* Reaps every child, as PID 1 must, and notes the agent's status. */
static void reap(void) {
  for (;;) {
    int st;
    pid_t p = waitpid(-1, &st, WNOHANG);
    if (p <= 0) return;
    if (p == agent_pid) {
      if (WIFEXITED(st))
        agent_status = WEXITSTATUS(st);
      else if (WIFSIGNALED(st))
        agent_status = 128 + WTERMSIG(st);
      else
        agent_status = 255;
      /* Children the agent left behind go with it. */
      signal_agent(SIGKILL);
    }
  }
}

static void drain_signals(void) {
  char tmp[64];
  while (read(sig_pipe[0], tmp, sizeof tmp) > 0) {
  }
  if (got_term) {
    got_term = 0;
    signal_agent(SIGTERM);
  }
  if (got_int) {
    got_int = 0;
    signal_agent(SIGINT);
  }
  reap();
}

/* The agent */

static void close_from(int lowest) {
  struct rlimit rl;
  long max = 65536;
  if (getrlimit(RLIMIT_NOFILE, &rl) == 0 && rl.rlim_cur != RLIM_INFINITY &&
      rl.rlim_cur < (rlim_t)max)
    max = (long)rl.rlim_cur;
  for (long fd = lowest; fd < max; fd++) close((int)fd);
}

static void reset_child_signals(void) {
  struct sigaction sa;
  memset(&sa, 0, sizeof sa);
  sa.sa_handler = SIG_DFL;
  sigemptyset(&sa.sa_mask);
  sigaction(SIGCHLD, &sa, NULL);
  sigaction(SIGTERM, &sa, NULL);
  sigaction(SIGINT, &sa, NULL);
  sigaction(SIGPIPE, &sa, NULL);
}

#define PROBE_ARGS 7

static int run_probe(int port, const char **paths);

/* Starts the agent (or the probe) in a process group of its own, with pipes
   for its standard input and output and nothing else open above 2. */
static void start_agent(char **argv, const char **probe, int probe_port) {
  int in_pipe[2], out_pipe[2];
  if (pipe(in_pipe) < 0 || pipe(out_pipe) < 0) die("pipe");
  pid_t pid = fork();
  if (pid < 0) die("fork");
  if (pid == 0) {
    reset_child_signals();
    if (setpgid(0, 0) < 0) _exit(126);
    if (dup2(in_pipe[0], 0) < 0 || dup2(out_pipe[1], 1) < 0) _exit(126);
    close_from(3);
    if (probe != NULL) _exit(run_probe(probe_port, probe));
    execvp(argv[0], argv);
    fprintf(stderr, "bactrian relay: cannot start %s: %s\n", argv[0],
            strerror(errno));
    _exit(errno == ENOENT ? 127 : 126);
  }
  /* Set here as well, so a signal that arrives before the child has run
     setpgid still reaches the group. */
  if (setpgid(pid, pid) < 0 && errno != EACCES && errno != ESRCH)
    die("setpgid");
  agent_pid = pid;
  close_fd(&in_pipe[0]);
  close_fd(&out_pipe[1]);
  agent_in = in_pipe[1];
  agent_out = out_pipe[0];
  set_nonblock(agent_in);
  set_nonblock(agent_out);
  if (fcntl(agent_in, F_SETFD, FD_CLOEXEC) < 0 ||
      fcntl(agent_out, F_SETFD, FD_CLOEXEC) < 0)
    die("fcntl");
}

/* Listeners and connections */

static void listen_on(int port) {
  int fd = socket(AF_INET, SOCK_STREAM | SOCK_CLOEXEC | SOCK_NONBLOCK, 0);
  if (fd < 0) die("socket");
  int one = 1;
  if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one) < 0)
    die("setsockopt");
  struct sockaddr_in a;
  memset(&a, 0, sizeof a);
  a.sin_family = AF_INET;
  a.sin_port = htons((uint16_t)port);
  a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  if (bind(fd, (struct sockaddr *)&a, sizeof a) < 0) die("bind");
  if (listen(fd, 128) < 0) die("listen");
  listeners[nforwards] = fd;
  ports[nforwards] = port;
  nforwards++;
}

static struct conn *find_conn(uint32_t channel) {
  for (int i = 0; i < MAX_CONNS; i++)
    if (conns[i].fd >= 0 && conns[i].channel == channel) return &conns[i];
  return NULL;
}

static void free_conn(struct conn *c, int tell) {
  if (tell) frame(c->channel, K_CLOSE, NULL, 0);
  close_fd(&c->fd);
  c->sink.off = c->sink.len = 0;
}

static void accept_on(int i) {
  int fd = accept4(listeners[i], NULL, NULL, SOCK_CLOEXEC | SOCK_NONBLOCK);
  if (fd < 0) {
    if (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR &&
        errno != ECONNABORTED)
      log_failure(ports[i], "relay", strerror(errno));
    return;
  }
  for (int k = 0; k < MAX_CONNS; k++) {
    struct conn *c = &conns[k];
    if (c->fd >= 0) continue;
    c->fd = fd;
    c->channel = next_channel;
    next_channel += 2;
    c->port = ports[i];
    c->eof_in = c->shut = c->eof_out = 0;
    unsigned char p[2] = {(unsigned char)(ports[i] >> 8),
                          (unsigned char)ports[i]};
    frame(c->channel, K_OPEN, p, 2);
    return;
  }
  log_failure(ports[i], "relay", "too many open connections");
  close_fd(&fd);
}

/* A connection is done when both directions have ended. */
static void maybe_finish(struct conn *c) {
  if (c->eof_out && c->shut) free_conn(c, 1);
}

static void read_conn(struct conn *c) {
  unsigned char tmp[CHUNK];
  ssize_t n = read(c->fd, tmp, sizeof tmp);
  if (n > 0) {
    frame(c->channel, K_DATA, tmp, (size_t)n);
  } else if (n == 0) {
    frame(c->channel, K_EOF, NULL, 0);
    c->eof_out = 1;
    maybe_finish(c);
  } else if (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR) {
    free_conn(c, 1);
  }
}

static void write_conn(struct conn *c) {
  ssize_t n = write(c->fd, c->sink.data + c->sink.off, buf_used(&c->sink));
  if (n > 0) {
    buf_take(&c->sink, (size_t)n);
  } else if (n < 0 && errno != EAGAIN && errno != EWOULDBLOCK &&
             errno != EINTR) {
    free_conn(c, 1);
    return;
  }
  if (buf_used(&c->sink) == 0 && c->eof_in && !c->shut) {
    if (shutdown(c->fd, SHUT_WR) < 0 && errno != ENOTCONN) {
      free_conn(c, 1);
      return;
    }
    c->shut = 1;
    maybe_finish(c);
  }
}

/* Frames from bactrian */

static unsigned char in_frame[HEADER + MAX_PAYLOAD];
static size_t in_have;     /* bytes of the current frame read so far */
static int in_pending;     /* a whole frame waits for room at its sink */
static long last_heard;    /* when a frame last came, or reading resumed */

static long now_ms(void);

__attribute__((noreturn)) static void protocol_error(const char *why) {
  log_failure(0, "relay", why);
  fprintf(stderr, "bactrian relay: protocol error: %s\n", why);
  signal_agent(SIGKILL);
  /* Best effort: the log line goes out before the relay stops. */
  int flags = fcntl(1, F_GETFL);
  if (flags >= 0) fcntl(1, F_SETFL, flags & ~O_NONBLOCK);
  while (buf_used(&out) > 0) {
    ssize_t n = write(1, out.data + out.off, buf_used(&out));
    if (n <= 0) break;
    buf_take(&out, (size_t)n);
  }
  exit(EXIT_PROTOCOL);
}

static size_t in_needed(void) {
  if (in_have < HEADER) return HEADER;
  return HEADER + get_u32(in_frame + 5);
}

/* Delivers the whole frame in in_frame. 0 when its sink is full, so the frame
   stays and reading stops until the sink drains. */
static int deliver(void) {
  uint32_t channel = get_u32(in_frame);
  unsigned kind = in_frame[4];
  size_t n = get_u32(in_frame + 5);
  const unsigned char *p = in_frame + HEADER;
  if (channel == 0) {
    switch (kind) {
      case K_DATA:
        if (agent_in < 0 || agent_in_eof) return 1;
        if (buf_room(&to_agent) < n) return 0;
        buf_put(&to_agent, p, n);
        return 1;
      case K_EOF:
        agent_in_eof = 1;
        if (buf_used(&to_agent) == 0) close_fd(&agent_in);
        return 1;
      case K_HEARTBEAT:
        return 1;
      default:
        protocol_error("unexpected frame kind on channel 0");
    }
  }
  if (channel % 2 == 0) protocol_error("frame on an even channel");
  struct conn *c = find_conn(channel);
  switch (kind) {
    case K_DATA:
      if (c == NULL || c->eof_in) return 1;
      if (buf_room(&c->sink) < n) return 0;
      buf_put(&c->sink, p, n);
      return 1;
    case K_EOF:
      if (c != NULL) {
        c->eof_in = 1;
        write_conn(c);
      }
      return 1;
    case K_CLOSE:
      if (c != NULL) free_conn(c, 0);
      return 1;
    default:
      protocol_error("unexpected frame kind on a connection channel");
  }
  return 1;
}

/* Reads what the current frame still needs, and delivers whole frames. */
static void read_frames(void) {
  size_t need = in_needed();
  ssize_t n = read(0, in_frame + in_have, need - in_have);
  if (n == 0) {
    /* bactrian is gone: nobody can read the agent's output any more. */
    fprintf(stderr, "bactrian relay: standard input closed\n");
    signal_agent(SIGKILL);
    exit(EXIT_PROTOCOL);
  }
  if (n < 0) {
    if (errno == EAGAIN || errno == EWOULDBLOCK || errno == EINTR) return;
    die("read");
  }
  in_have += (size_t)n;
  if (in_have == HEADER) {
    if (in_frame[4] > K_HEARTBEAT) protocol_error("unknown frame kind");
    if (get_u32(in_frame + 5) > MAX_PAYLOAD) protocol_error("frame too long");
  }
  if (in_have >= HEADER && in_have == in_needed()) {
    last_heard = now_ms();
    if (deliver())
      in_have = 0;
    else
      in_pending = 1;
  }
}

/* A frame that waits for room is the agent's backpressure, not bactrian's
   silence, so the wait does not count towards LIVENESS_MS. */
static void retry_pending(void) {
  if (in_pending && deliver()) {
    in_pending = 0;
    in_have = 0;
    last_heard = now_ms();
  }
}

/* The agent's pipes */

static int agent_out_eof;

static void read_agent(void) {
  unsigned char tmp[CHUNK];
  ssize_t n = read(agent_out, tmp, sizeof tmp);
  if (n > 0) {
    frame(0, K_DATA, tmp, (size_t)n);
  } else if (n == 0 || (errno != EAGAIN && errno != EWOULDBLOCK &&
                        errno != EINTR)) {
    frame(0, K_EOF, NULL, 0);
    agent_out_eof = 1;
    close_fd(&agent_out);
  }
}

static void write_agent(void) {
  ssize_t n =
      write(agent_in, to_agent.data + to_agent.off, buf_used(&to_agent));
  if (n > 0) {
    buf_take(&to_agent, (size_t)n);
  } else if (n < 0 && errno != EAGAIN && errno != EWOULDBLOCK &&
             errno != EINTR) {
    /* The agent closed its standard input: what it did not read is lost. */
    to_agent.off = to_agent.len = 0;
    agent_in_eof = 1;
  }
  if (buf_used(&to_agent) == 0 && agent_in_eof) close_fd(&agent_in);
}

static void flush_out(void) {
  ssize_t n = write(1, out.data + out.off, buf_used(&out));
  if (n > 0)
    buf_take(&out, (size_t)n);
  else if (n < 0 && errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR) {
    fprintf(stderr, "bactrian relay: standard output: %s\n", strerror(errno));
    signal_agent(SIGKILL);
    exit(EXIT_PROTOCOL);
  }
}

static long now_ms(void) {
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) < 0) die("clock_gettime");
  return (long)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

/* The loop */

enum { MAX_POLL = 4 + MAX_FORWARDS + MAX_CONNS };

static int run_loop(void) {
  long drain_until = -1;
  for (;;) {
    if (agent_status >= 0) {
      if (drain_until < 0) drain_until = now_ms() + DRAIN_MS;
      /* The agent is gone: once its output is read (or a process that left
         its group still holds the pipe past the deadline), the run ends. */
      if (agent_out_eof || now_ms() >= drain_until) {
        if (!agent_out_eof) frame(0, K_EOF, NULL, 0);
        unsigned char st[4];
        put_u32(st, (uint32_t)agent_status);
        frame(0, K_EXIT, st, 4);
        int flags = fcntl(1, F_GETFL);
        if (flags < 0 || fcntl(1, F_SETFL, flags & ~O_NONBLOCK) < 0)
          die("fcntl");
        while (buf_used(&out) > 0) {
          ssize_t n = write(1, out.data + out.off, buf_used(&out));
          if (n < 0 && errno == EINTR) continue;
          if (n <= 0) break;
          buf_take(&out, (size_t)n);
        }
        return agent_status;
      }
    }
    struct pollfd fds[MAX_POLL];
    int nf = 0;
    int ix_sig = nf;
    fds[nf++] = (struct pollfd){sig_pipe[0], POLLIN, 0};
    int ix_in = -1, ix_out = -1, ix_aout = -1, ix_ain = -1;
    if (!in_pending) {
      ix_in = nf;
      fds[nf++] = (struct pollfd){0, POLLIN, 0};
    }
    if (buf_used(&out) > 0) {
      ix_out = nf;
      fds[nf++] = (struct pollfd){1, POLLOUT, 0};
    }
    if (agent_out >= 0 && out_has_room(CHUNK)) {
      ix_aout = nf;
      fds[nf++] = (struct pollfd){agent_out, POLLIN, 0};
    }
    if (agent_in >= 0 && buf_used(&to_agent) > 0) {
      ix_ain = nf;
      fds[nf++] = (struct pollfd){agent_in, POLLOUT, 0};
    }
    int ix_listen = nf;
    int accepting = agent_status < 0 && out_has_room(0);
    for (int i = 0; i < nforwards; i++)
      fds[nf++] = (struct pollfd){listeners[i], accepting ? POLLIN : 0, 0};
    int ix_conn[MAX_CONNS];
    for (int i = 0; i < MAX_CONNS; i++) {
      struct conn *c = &conns[i];
      ix_conn[i] = -1;
      if (c->fd < 0) continue;
      short ev = 0;
      if (!c->eof_out && out_has_room(CHUNK)) ev |= POLLIN;
      if (buf_used(&c->sink) > 0) ev |= POLLOUT;
      ix_conn[i] = nf;
      fds[nf++] = (struct pollfd){c->fd, ev, 0};
    }
    long now = now_ms();
    if (in_pending) last_heard = now;
    if (now - last_heard >= LIVENESS_MS) {
      log_failure(0, "relay", "nothing from bactrian for 10 seconds");
      fprintf(stderr,
              "bactrian relay: nothing from bactrian for %d seconds, so the "
              "agent is stopped\n",
              LIVENESS_MS / 1000);
      signal_agent(SIGKILL);
      exit(EXIT_PROTOCOL);
    }
    long left = last_heard + LIVENESS_MS - now;
    if (drain_until >= 0 && drain_until - now < left) left = drain_until - now;
    int timeout = left < 0 ? 0 : (int)left;
    int r = poll(fds, (nfds_t)nf, timeout);
    if (r < 0) {
      if (errno == EINTR) continue;
      die("poll");
    }
    if (fds[ix_sig].revents != 0) drain_signals();
    if (ix_out >= 0 && fds[ix_out].revents != 0) flush_out();
    if (ix_ain >= 0 && fds[ix_ain].revents != 0 && agent_in >= 0) write_agent();
    retry_pending();
    for (int i = 0; i < MAX_CONNS; i++) {
      struct conn *c = &conns[i];
      int k = ix_conn[i];
      if (k < 0 || c->fd < 0 || fds[k].revents == 0) continue;
      if ((fds[k].revents & (POLLOUT | POLLERR | POLLHUP)) &&
          buf_used(&c->sink) > 0)
        write_conn(c);
      if (c->fd >= 0 && (fds[k].revents & (POLLIN | POLLERR | POLLHUP)) &&
          !c->eof_out && out_has_room(CHUNK))
        read_conn(c);
    }
    retry_pending();
    if (ix_aout >= 0 && fds[ix_aout].revents != 0 && agent_out >= 0 &&
        out_has_room(CHUNK))
      read_agent();
    for (int i = 0; i < nforwards; i++)
      if (fds[ix_listen + i].revents & POLLIN) accept_on(i);
    if (ix_in >= 0 && fds[ix_in].revents != 0 && !in_pending) read_frames();
  }
}

/* The probe */

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

/* Arguments */

static int parse_port(const char *s) {
  char *end;
  errno = 0;
  long v = strtol(s, &end, 10);
  if (errno != 0 || *end != '\0' || end == s || v < 1 || v > 65535) return -1;
  return (int)v;
}

static void usage(void) {
  fprintf(stderr,
          "usage: relay --forward PORT... -- AGENT ARGV...\n"
          "       relay --forward PORT --probe OUTSIDE INSIDE DENIED MASKED "
          "MASKED_CASE ROOT OTHER_RUN\n");
  exit(EXIT_PROTOCOL);
}

int main(int argc, char **argv) {
  int i = 1;
  const char *probe[PROBE_ARGS];
  int probing = 0;
  int forward_ports[MAX_FORWARDS], nports = 0;
  while (i < argc) {
    if (strcmp(argv[i], "--forward") == 0 && i + 1 < argc) {
      int p = parse_port(argv[i + 1]);
      if (p < 0 || nports == MAX_FORWARDS) usage();
      forward_ports[nports++] = p;
      i += 2;
    } else if (strcmp(argv[i], "--probe") == 0 && i + PROBE_ARGS < argc) {
      probing = 1;
      for (int k = 0; k < PROBE_ARGS; k++) probe[k] = argv[i + 1 + k];
      i += 1 + PROBE_ARGS;
    } else if (strcmp(argv[i], "--") == 0) {
      i++;
      break;
    } else {
      usage();
    }
  }
  if (probing ? (nports != 1 || i < argc) : i >= argc) usage();
  /* exec makes the agent dumpable again: this holds for the relay alone. */
  if (prctl(PR_SET_DUMPABLE, 0, 0, 0, 0) < 0) die("prctl");
  set_nonblock(0);
  set_nonblock(1);
  buf_init(&out, OUT_CAP);
  buf_init(&to_agent, SINK_CAP);
  for (int k = 0; k < MAX_CONNS; k++) {
    conns[k].fd = -1;
    buf_init(&conns[k].sink, SINK_CAP);
  }
  install_signals();
  last_heard = now_ms();
  /* Listeners first, so the agent can connect at once. */
  for (int k = 0; k < nports; k++) listen_on(forward_ports[k]);
  start_agent(argv + i, probing ? probe : NULL, nports > 0 ? forward_ports[0] : 0);
  return run_loop();
}
