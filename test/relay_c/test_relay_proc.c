/* The relay (vm/relay.c) from outside: the compiled binary, started as the
   container starts it and driven over pipes and a loopback socket as
   bactrian's bridge drives it. Frames are spelt out here, not taken from the
   relay's source, so these tests check the wire format itself. The relay is
   Linux-only (prctl, accept4), so dune builds and runs this on Linux alone,
   with the relay built with a liveness timeout of LIVENESS_MS.

   usage: test_relay_proc RELAY

   Each test runs in a process of its own with an alarm, and whatever it left
   in its process group is killed afterwards. */

#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <poll.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#ifndef LIVENESS_MS
#error "LIVENESS_MS must match the relay under test"
#endif

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
#define SINK_CAP (256 * 1024)
#define MAX_CONNS 64
#define EXIT_PROTOCOL 70

static const char *relay_path;
static int failures;

#define CHECK(cond)                                                    \
  do {                                                                 \
    if (!(cond)) {                                                     \
      fprintf(stderr, "%s:%d: check failed: %s\n", __FILE__, __LINE__, \
              #cond);                                                  \
      failures++;                                                      \
    }                                                                  \
  } while (0)

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

static long now_ms(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (long)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

static void die(const char *what) {
  perror(what);
  exit(2);
}

/* The relay in a child */

struct relay {
  pid_t pid;
  int to; /* bactrian to the relay's standard input */
  int from; /* the relay's standard output */
  unsigned char buf[1 << 20];
  size_t have;
};

struct fr {
  uint32_t channel;
  unsigned kind;
  size_t len;
  unsigned char payload[MAX_PAYLOAD];
};

static long ms(void) { return now_ms(); }

/* Starts the relay binary with [argv] (argv[0] is "relay"). */
static void start_relay(struct relay *r, char **argv) {
  int in[2], outp[2];
  if (pipe(in) < 0 || pipe(outp) < 0) die("pipe");
  pid_t pid = fork();
  if (pid < 0) die("fork");
  if (pid == 0) {
    dup2(in[0], 0);
    dup2(outp[1], 1);
    close(in[0]);
    close(in[1]);
    close(outp[0]);
    close(outp[1]);
    alarm(40);
    execv(relay_path, argv);
    _exit(127);
  }
  close(in[0]);
  close(outp[1]);
  r->pid = pid;
  r->to = in[1];
  r->from = outp[0];
  r->have = 0;
  int fl = fcntl(r->from, F_GETFL);
  fcntl(r->from, F_SETFL, fl | O_NONBLOCK);
}

static void send_raw(struct relay *r, const void *p, size_t n) {
  const unsigned char *c = p;
  while (n > 0) {
    ssize_t w = write(r->to, c, n);
    if (w < 0) {
      if (errno == EINTR) continue;
      return;
    }
    c += w;
    n -= (size_t)w;
  }
}

static void send_frame(struct relay *r, uint32_t channel, unsigned kind,
                       const void *payload, size_t n) {
  unsigned char h[HEADER];
  put_u32(h, channel);
  h[4] = (unsigned char)kind;
  put_u32(h + 5, (uint32_t)n);
  send_raw(r, h, HEADER);
  if (n > 0) send_raw(r, payload, n);
}

/* The next frame from the relay within [within] milliseconds: 1, or 0 when
   none came, or -1 when the relay closed its output. */
static int next_frame(struct relay *r, struct fr *f, long within) {
  long deadline = ms() + within;
  for (;;) {
    if (r->have >= HEADER) {
      size_t n = get_u32(r->buf + 5);
      if (r->have >= HEADER + n) {
        f->channel = get_u32(r->buf);
        f->kind = r->buf[4];
        f->len = n;
        memcpy(f->payload, r->buf + HEADER, n);
        memmove(r->buf, r->buf + HEADER + n, r->have - HEADER - n);
        r->have -= HEADER + n;
        return 1;
      }
    }
    long left = deadline - ms();
    if (left < 0) return 0;
    struct pollfd p = {r->from, POLLIN, 0};
    if (poll(&p, 1, (int)left) <= 0) continue;
    ssize_t n = read(r->from, r->buf + r->have, sizeof r->buf - r->have);
    if (n == 0) return -1;
    if (n > 0) r->have += (size_t)n;
  }
}

/* Reads frames until [kind] on [channel], appending the payloads of data
   frames to [data] (if given). 1 when it came. */
static int until_frame(struct relay *r, struct fr *f, unsigned kind,
                       uint32_t channel, long within) {
  long deadline = ms() + within;
  for (;;) {
    long left = deadline - ms();
    if (left < 0) return 0;
    if (next_frame(r, f, left) != 1) return 0;
    if (f->kind == kind && f->channel == channel) return 1;
  }
}

/* Everything the relay says on channel 0 until its exit frame. Returns the
   exit status, or -1 when no exit frame came. */
static int collect(struct relay *r, unsigned char *data, size_t *data_len,
                   int *saw_eof, long within) {
  struct fr f;
  long deadline = ms() + within;
  *data_len = 0;
  *saw_eof = 0;
  for (;;) {
    long left = deadline - ms();
    if (left < 0 || next_frame(r, &f, left) != 1) return -1;
    if (f.kind == K_DATA && f.channel == 0) {
      if (data != NULL) memcpy(data + *data_len, f.payload, f.len);
      *data_len += f.len;
    } else if (f.kind == K_EOF && f.channel == 0) {
      *saw_eof = 1;
    } else if (f.kind == K_EXIT) {
      return (int)get_u32(f.payload);
    }
  }
}

/* How the relay ended: its exit code, or -1 if it is still there after
   [within] milliseconds (and then it is killed). */
static int relay_exit(struct relay *r, long within) {
  long deadline = ms() + within;
  int st;
  for (;;) {
    pid_t w = waitpid(r->pid, &st, WNOHANG);
    if (w == r->pid) return WIFEXITED(st) ? WEXITSTATUS(st) : 1000 + WTERMSIG(st);
    if (ms() > deadline) {
      kill(r->pid, SIGKILL);
      waitpid(r->pid, &st, 0);
      return -1;
    }
    usleep(10000);
  }
}

static char *forward_arg(char *buf, size_t cap, int port) {
  snprintf(buf, cap, "%d", port);
  return buf;
}

/* A port nobody is listening on, found by binding port 0 and letting go. */
static int free_port(void) {
  int fd = socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in a;
  memset(&a, 0, sizeof a);
  a.sin_family = AF_INET;
  a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  if (fd < 0 || bind(fd, (struct sockaddr *)&a, sizeof a) < 0) die("bind");
  socklen_t len = sizeof a;
  if (getsockname(fd, (struct sockaddr *)&a, &len) < 0) die("getsockname");
  close(fd);
  return ntohs(a.sin_port);
}

static int connect_to(int port) {
  int fd = socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in a;
  memset(&a, 0, sizeof a);
  a.sin_family = AF_INET;
  a.sin_port = htons((uint16_t)port);
  a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  for (int tries = 0; tries < 200; tries++) {
    if (connect(fd, (struct sockaddr *)&a, sizeof a) == 0) return fd;
    usleep(10000);
    close(fd);
    fd = socket(AF_INET, SOCK_STREAM, 0);
  }
  close(fd);
  return -1;
}

/* Reads up to [want] bytes from a blocking descriptor within [within]
   milliseconds. */
static size_t read_some(int fd, char *dst, size_t want, long within) {
  size_t have = 0;
  long deadline = ms() + within;
  while (have < want) {
    long left = deadline - ms();
    struct pollfd p = {fd, POLLIN, 0};
    if (left < 0 || poll(&p, 1, (int)left) <= 0) break;
    ssize_t n = read(fd, dst + have, want - have);
    if (n <= 0) break;
    have += (size_t)n;
  }
  return have;
}

static void t_echo(void) {
  struct relay r;
  char *argv[] = {"relay", "--", "cat", NULL};
  start_relay(&r, argv);
  send_frame(&r, 0, K_DATA, "hello ", 6);
  send_frame(&r, 0, K_DATA, "world", 5);
  send_frame(&r, 0, K_EOF, NULL, 0);
  unsigned char data[64];
  size_t n;
  int eof;
  CHECK(collect(&r, data, &n, &eof, 10000) == 0);
  CHECK(n == 11 && memcmp(data, "hello world", 11) == 0);
  CHECK(eof);
  CHECK(relay_exit(&r, 5000) == 0);
}

/* 300 KB each way through cat: split into frames of at most 64 KiB, in order,
   with the relay's buffers full part of the time. */
static void t_large_echo(void) {
  struct relay r;
  char *argv[] = {"relay", "--", "cat", NULL};
  start_relay(&r, argv);
  size_t total = 300 * 1000;
  unsigned char *src = malloc(total), *dst = malloc(total + MAX_PAYLOAD);
  for (size_t i = 0; i < total; i++) src[i] = (unsigned char)(i * 7 + i / 251);
  pid_t writer = fork();
  if (writer == 0) {
    for (size_t off = 0; off < total; off += 50000)
      send_frame(&r, 0, K_DATA, src + off, 50000);
    send_frame(&r, 0, K_EOF, NULL, 0);
    _exit(0);
  }
  size_t n;
  int eof;
  CHECK(collect(&r, dst, &n, &eof, 20000) == 0);
  CHECK(n == total && memcmp(src, dst, total) == 0);
  waitpid(writer, NULL, 0);
  CHECK(relay_exit(&r, 5000) == 0);
}

static void t_exit_status(void) {
  struct relay r;
  char *argv[] = {"relay", "--", "sh", "-c", "exit 7", NULL};
  start_relay(&r, argv);
  unsigned char data[8];
  size_t n;
  int eof;
  CHECK(collect(&r, data, &n, &eof, 10000) == 7);
  CHECK(relay_exit(&r, 5000) == 7);
}

static void t_killed_by_signal(void) {
  struct relay r;
  char *argv[] = {"relay", "--", "sh", "-c", "kill -9 $$", NULL};
  start_relay(&r, argv);
  unsigned char data[8];
  size_t n;
  int eof;
  CHECK(collect(&r, data, &n, &eof, 10000) == 128 + 9);
  CHECK(relay_exit(&r, 5000) == 128 + 9);
}

static void t_no_such_agent(void) {
  struct relay r;
  char *argv[] = {"relay", "--", "/nonexistent/agent", NULL};
  start_relay(&r, argv);
  unsigned char data[8];
  size_t n;
  int eof;
  CHECK(collect(&r, data, &n, &eof, 10000) == 127);
  CHECK(relay_exit(&r, 5000) == 127);
}

static void t_term_passes_on(void) {
  struct relay r;
  char *argv[] = {"relay", "--", "sh", "-c", "echo up; exec sleep 20", NULL};
  start_relay(&r, argv);
  struct fr f;
  CHECK(until_frame(&r, &f, K_DATA, 0, 10000));
  kill(r.pid, SIGTERM);
  unsigned char data[8];
  size_t n;
  int eof;
  CHECK(collect(&r, data, &n, &eof, 10000) == 128 + 15);
  CHECK(relay_exit(&r, 5000) == 128 + 15);
}

static void t_int_passes_on(void) {
  struct relay r;
  /* sleep replaces the shell, so it is sleep that gets the signal. With a
     foreground child instead, dash can hold a SIGINT that arrives while it
     forks or waits and leave sleep running. */
  char *argv[] = {"relay", "--", "sh", "-c", "echo up; exec sleep 20", NULL};
  start_relay(&r, argv);
  struct fr f;
  CHECK(until_frame(&r, &f, K_DATA, 0, 10000));
  kill(r.pid, SIGINT);
  unsigned char data[8];
  size_t n;
  int eof;
  int code = collect(&r, data, &n, &eof, 10000);
  CHECK(code == 128 + 2 || code == 130);
  CHECK(relay_exit(&r, 5000) == code);
}

static void t_hardening(void) {
  struct relay r;
  char *argv[] = {"relay", "--", "grep", "NoNewPrivs", "/proc/self/status",
                  NULL};
  start_relay(&r, argv);
  unsigned char data[128];
  size_t n;
  int eof;
  CHECK(collect(&r, data, &n, &eof, 10000) == 0);
  CHECK(n > 0 && data[n - 2] == '1');
  CHECK(relay_exit(&r, 5000) == 0);
}

/* The agent's descriptors above 2 are closed. */
static void t_fds_closed(void) {
  struct relay r;
  char *argv[] = {"relay", "--", "sh", "-c", "ls /proc/self/fd | wc -l", NULL};
  start_relay(&r, argv);
  unsigned char data[32];
  size_t n;
  int eof;
  CHECK(collect(&r, data, &n, &eof, 10000) == 0);
  data[n] = '\0';
  /* 0, 1, 2 and the directory handle ls itself holds. */
  CHECK(atoi((char *)data) <= 4);
  CHECK(relay_exit(&r, 5000) == 0);
}

static void t_multiplex(void) {
  int port = free_port();
  char p[16];
  struct relay r;
  char *argv[] = {"relay", "--forward", forward_arg(p, sizeof p, port),
                  "--", "sleep", "20", NULL};
  start_relay(&r, argv);
  int a = connect_to(port), b = connect_to(port);
  CHECK(a >= 0 && b >= 0);
  struct fr f;
  /* Odd channels in order of acceptance, each opened with the port. */
  CHECK(until_frame(&r, &f, K_OPEN, 1, 5000));
  CHECK(f.len == 2 && get_u32((unsigned char[]){0, 0, f.payload[0],
                                                f.payload[1]}) == (uint32_t)port);
  CHECK(until_frame(&r, &f, K_OPEN, 3, 5000));
  CHECK(write(a, "aaa", 3) == 3);
  CHECK(write(b, "bbbb", 4) == 4);
  size_t got_a = 0, got_b = 0;
  char ga[16], gb[16];
  while (got_a < 3 || got_b < 4) {
    if (next_frame(&r, &f, 5000) != 1) break;
    if (f.kind != K_DATA) continue;
    if (f.channel == 1 && got_a + f.len <= sizeof ga) {
      memcpy(ga + got_a, f.payload, f.len);
      got_a += f.len;
    } else if (f.channel == 3 && got_b + f.len <= sizeof gb) {
      memcpy(gb + got_b, f.payload, f.len);
      got_b += f.len;
    }
  }
  CHECK(got_a == 3 && memcmp(ga, "aaa", 3) == 0);
  CHECK(got_b == 4 && memcmp(gb, "bbbb", 4) == 0);
  /* Replies go to the right connection. */
  send_frame(&r, 3, K_DATA, "to-b", 4);
  send_frame(&r, 1, K_DATA, "to-a", 4);
  char buf[16];
  CHECK(read_some(a, buf, 4, 5000) == 4 && memcmp(buf, "to-a", 4) == 0);
  CHECK(read_some(b, buf, 4, 5000) == 4 && memcmp(buf, "to-b", 4) == 0);
  /* The client's end of file is an eof frame, and bactrian's eof then ends
     the connection with a close frame. */
  shutdown(a, SHUT_WR);
  CHECK(until_frame(&r, &f, K_EOF, 1, 5000));
  send_frame(&r, 1, K_EOF, NULL, 0);
  CHECK(until_frame(&r, &f, K_CLOSE, 1, 5000));
  CHECK(read_some(a, buf, 1, 5000) == 0);
  /* A close frame from bactrian ends the other one at once. */
  send_frame(&r, 3, K_CLOSE, NULL, 0);
  CHECK(read_some(b, buf, 1, 5000) == 0);
  kill(r.pid, SIGTERM);
  CHECK(relay_exit(&r, 10000) == 128 + 15);
}

/* More connections than MAX_CONNS: the extra one is closed and logged. */
static void t_too_many_connections(void) {
  int port = free_port();
  char p[16];
  struct relay r;
  char *argv[] = {"relay", "--forward", forward_arg(p, sizeof p, port),
                  "--", "sleep", "20", NULL};
  start_relay(&r, argv);
  int fds[MAX_CONNS + 1];
  for (int i = 0; i < MAX_CONNS + 1; i++) {
    fds[i] = connect_to(port);
    CHECK(fds[i] >= 0);
  }
  struct fr f;
  int opens = 0, logged = 0;
  long deadline = ms() + 8000;
  while (ms() < deadline && !(opens >= MAX_CONNS && logged)) {
    if (next_frame(&r, &f, 500) != 1) continue;
    if (f.kind == K_OPEN) opens++;
    if (f.kind == K_LOG && memmem(f.payload, f.len, "too many open", 13) != NULL)
      logged = 1;
  }
  CHECK(opens == MAX_CONNS);
  CHECK(logged);
  kill(r.pid, SIGTERM);
  CHECK(relay_exit(&r, 10000) == 128 + 15);
}

/* A frame bactrian should not send ends the run with 70, a log frame saying
   why, and the agent killed. */
static void bad_frame(const unsigned char *bytes, size_t n, const char *why) {
  struct relay r;
  char *argv[] = {"relay", "--", "sleep", "20", NULL};
  start_relay(&r, argv);
  send_raw(&r, bytes, n);
  struct fr f;
  CHECK(until_frame(&r, &f, K_LOG, 0, 10000));
  CHECK(memmem(f.payload, f.len, why, strlen(why)) != NULL);
  CHECK(relay_exit(&r, 10000) == EXIT_PROTOCOL);
}

static void t_bad_kind(void) {
  const unsigned char b[] = {0, 0, 0, 0, 9, 0, 0, 0, 0};
  bad_frame(b, sizeof b, "unknown frame kind");
}

static void t_too_long(void) {
  const unsigned char b[] = {0, 0, 0, 0, K_DATA, 0, 1, 0, 1};
  bad_frame(b, sizeof b, "frame too long");
}

static void t_even_channel(void) {
  const unsigned char b[] = {0, 0, 0, 2, K_DATA, 0, 0, 0, 0};
  bad_frame(b, sizeof b, "even channel");
}

static void t_exit_from_bactrian(void) {
  const unsigned char b[] = {0, 0, 0, 0, K_EXIT, 0, 0, 0, 0};
  bad_frame(b, sizeof b, "unexpected frame kind on channel 0");
}

static void t_open_from_bactrian(void) {
  const unsigned char b[] = {0, 0, 0, 1, K_OPEN, 0, 0, 0, 0};
  bad_frame(b, sizeof b, "unexpected frame kind on a connection channel");
}

/* A frame cut in half and then completed is still one frame. */
static void t_split_frame(void) {
  struct relay r;
  char *argv[] = {"relay", "--", "cat", NULL};
  start_relay(&r, argv);
  unsigned char h[HEADER];
  put_u32(h, 0);
  h[4] = K_DATA;
  put_u32(h + 5, 5);
  send_raw(&r, h, 3);
  usleep(100000);
  send_raw(&r, h + 3, HEADER - 3);
  send_raw(&r, "he", 2);
  usleep(100000);
  send_raw(&r, "llo", 3);
  send_frame(&r, 0, K_EOF, NULL, 0);
  unsigned char data[16];
  size_t n;
  int eof;
  CHECK(collect(&r, data, &n, &eof, 10000) == 0);
  CHECK(n == 5 && memcmp(data, "hello", 5) == 0);
}

/* The end of the relay's input is bactrian gone: the agent is killed. */
static void t_input_closed(void) {
  char file[] = "/tmp/relay-test-XXXXXX";
  int fd = mkstemp(file);
  close(fd);
  struct relay r;
  char script[128];
  snprintf(script, sizeof script, "echo $$ > %s; sleep 20", file);
  char *argv[] = {"relay", "--", "sh", "-c", script, NULL};
  start_relay(&r, argv);
  usleep(500000);
  close(r.to);
  CHECK(relay_exit(&r, 10000) == EXIT_PROTOCOL);
  FILE *f = fopen(file, "r");
  int pid = 0;
  CHECK(f != NULL && fscanf(f, "%d", &pid) == 1);
  if (f != NULL) fclose(f);
  unlink(file);
  usleep(200000);
  CHECK(pid > 0 && kill(pid, 0) < 0 && errno == ESRCH);
}

/* A relay no one writes to stops after LIVENESS_MS, not before. */
static void t_liveness(void) {
  struct relay r;
  char *argv[] = {"relay", "--", "sleep", "20", NULL};
  start_relay(&r, argv);
  long start = ms();
  struct fr f;
  CHECK(until_frame(&r, &f, K_LOG, 0, 10000));
  CHECK(memmem(f.payload, f.len, "nothing from bactrian", 21) != NULL);
  CHECK(relay_exit(&r, 5000) == EXIT_PROTOCOL);
  long took = ms() - start;
  CHECK(took >= LIVENESS_MS - 100 && took < LIVENESS_MS + 2500);
}

/* Heartbeat frames keep it alive for longer than LIVENESS_MS. */
static void t_frame_heartbeats(void) {
  struct relay r;
  char *argv[] = {"relay", "--", "sleep", "20", NULL};
  start_relay(&r, argv);
  long start = ms();
  while (ms() - start < 3 * LIVENESS_MS) {
    send_frame(&r, 0, K_HEARTBEAT, NULL, 0);
    usleep(300000);
  }
  CHECK(waitpid(r.pid, NULL, WNOHANG) == 0);
  /* Then they stop. */
  CHECK(relay_exit(&r, LIVENESS_MS + 3000) == EXIT_PROTOCOL);
}

/* Fills the relay's input until it takes no more, as an agent that never
   reads its standard input leaves it. */
static void fill_input(struct relay *r) {
  int fl = fcntl(r->to, F_GETFL);
  fcntl(r->to, F_SETFL, fl | O_NONBLOCK);
  unsigned char block[HEADER + 16384];
  put_u32(block, 0);
  block[4] = K_DATA;
  put_u32(block + 5, 16384);
  memset(block + HEADER, 'x', 16384);
  long quiet_since = ms();
  size_t sent = 0, off = 0; /* a partly written frame is finished first */
  while (ms() - quiet_since < 700) {
    ssize_t w = write(r->to, block + off, sizeof block - off);
    if (w > 0) {
      sent += (size_t)w;
      off = (off + (size_t)w) % sizeof block;
      quiet_since = ms();
    } else {
      usleep(20000);
    }
  }
  /* More than the relay's queue for the agent plus a pipe's worth. */
  CHECK(sent > SINK_CAP);
}

/* Without a heartbeat file, a frame waiting for an agent that reads nothing
   holds the timer off: the backpressure is the agent's, not bactrian's. */
static void t_backpressure_without_file(void) {
  struct relay r;
  char *argv[] = {"relay", "--", "sleep", "20", NULL};
  start_relay(&r, argv);
  fill_input(&r);
  usleep(3 * LIVENESS_MS * 1000);
  CHECK(waitpid(r.pid, NULL, WNOHANG) == 0);
  kill(r.pid, SIGTERM);
  CHECK(relay_exit(&r, 10000) == 128 + 15);
}

static void touch_now(const char *path) {
  CHECK(utimensat(AT_FDCWD, path, NULL, 0) == 0);
}

/* With a heartbeat file the same stuck frame no longer holds the timer: the
   file's changes keep the relay alive, and when they stop it ends. */
static void t_backpressure_with_file(void) {
  char dir[] = "/tmp/relay-hb-XXXXXX";
  CHECK(mkdtemp(dir) != NULL);
  char file[64];
  snprintf(file, sizeof file, "%s/beat", dir);
  close(open(file, O_CREAT | O_WRONLY, 0600));
  struct relay r;
  char *argv[] = {"relay", "--heartbeat", file, "--", "sleep", "20", NULL};
  start_relay(&r, argv);
  fill_input(&r);
  long start = ms();
  while (ms() - start < 3 * LIVENESS_MS) {
    touch_now(file);
    usleep(300000);
  }
  CHECK(waitpid(r.pid, NULL, WNOHANG) == 0);
  long stopped = ms();
  CHECK(relay_exit(&r, LIVENESS_MS + 3000) == EXIT_PROTOCOL);
  long took = ms() - stopped;
  CHECK(took >= LIVENESS_MS - 1200 && took < LIVENESS_MS + 2500);
  unlink(file);
  rmdir(dir);
}

/* Frames do not count once a heartbeat file is watched: they may be old. */
static void t_frames_do_not_count_with_file(void) {
  char dir[] = "/tmp/relay-hb-XXXXXX";
  CHECK(mkdtemp(dir) != NULL);
  char file[64];
  snprintf(file, sizeof file, "%s/beat", dir);
  close(open(file, O_CREAT | O_WRONLY, 0600));
  struct relay r;
  char *argv[] = {"relay", "--heartbeat", file, "--", "sleep", "20", NULL};
  start_relay(&r, argv);
  long start = ms();
  int code = -1, st;
  while (ms() - start < 3 * LIVENESS_MS) {
    send_frame(&r, 0, K_HEARTBEAT, NULL, 0);
    usleep(200000);
    if (waitpid(r.pid, &st, WNOHANG) == r.pid) {
      code = WIFEXITED(st) ? WEXITSTATUS(st) : -2;
      break;
    }
  }
  CHECK(code == EXIT_PROTOCOL);
  CHECK(ms() - start < LIVENESS_MS + 2500);
  unlink(file);
  rmdir(dir);
}

/* A heartbeat file that is gone is not a sign of life. */
static void t_file_removed(void) {
  char dir[] = "/tmp/relay-hb-XXXXXX";
  CHECK(mkdtemp(dir) != NULL);
  char file[64];
  snprintf(file, sizeof file, "%s/beat", dir);
  close(open(file, O_CREAT | O_WRONLY, 0600));
  struct relay r;
  char *argv[] = {"relay", "--heartbeat", file, "--", "sleep", "20", NULL};
  start_relay(&r, argv);
  usleep(300000);
  unlink(file);
  CHECK(relay_exit(&r, LIVENESS_MS + 3000) == EXIT_PROTOCOL);
  rmdir(dir);
}

/* When the silence comes with agent output still queued, the log frame is not
   written behind it: it could be cut. What reaches the reader is data frames
   only, the last of them perhaps cut by the exit. */
static void t_liveness_with_output_queued(void) {
  struct relay r;
  char *argv[] = {"relay", "--", "sh", "-c",
                  "head -c 3000000 /dev/zero | tr '\\0' 'y'; exec sleep 20",
                  NULL};
  start_relay(&r, argv);
  /* Not read: the pipe and the relay's queue fill up. */
  CHECK(relay_exit(&r, LIVENESS_MS + 3000) == EXIT_PROTOCOL);
  struct fr f;
  int frames = 0, others = 0, got;
  while ((got = next_frame(&r, &f, 500)) == 1) {
    frames++;
    if (f.kind != K_DATA) others++;
  }
  CHECK(got == -1);
  CHECK(frames > 0);
  CHECK(others == 0);
}

/* The agent is gone and nobody reads what it wrote: the relay gives up after
   LIVENESS_MS without a byte taken, and not before, instead of holding its
   container for ever. */
static void t_exit_flush_bounded(void) {
  struct relay r;
  char *argv[] = {"relay", "--", "sh", "-c",
                  "head -c 300000 /dev/zero | tr '\\0' 'y'; exit 5", NULL};
  start_relay(&r, argv);
  long start = ms();
  CHECK(relay_exit(&r, LIVENESS_MS + 3000) == 5);
  long took = ms() - start;
  CHECK(took >= LIVENESS_MS - 100 && took < LIVENESS_MS + 2500);
}

/* The agent writes more than the relay holds, and bactrian reads late: all of
   it arrives, in order, once it does. */
static void t_output_backpressure(void) {
  struct relay r;
  char *argv[] = {"relay", "--", "sh", "-c",
                  "head -c 5000000 /dev/zero | tr '\\0' 'y'", NULL};
  start_relay(&r, argv);
  usleep(1000000);
  /* Not read for a second: the relay's output queue is full by now. */
  size_t n;
  int eof;
  CHECK(collect(&r, NULL, &n, &eof, 20000) == 0);
  CHECK(n == 5000000);
  CHECK(eof);
  CHECK(relay_exit(&r, 5000) == 0);
}

/* The agent's output is not framed past the exit: the exit frame is last. */
static void t_exit_is_last(void) {
  struct relay r;
  char *argv[] = {"relay", "--", "sh", "-c", "echo a; echo b; exit 3", NULL};
  start_relay(&r, argv);
  struct fr f;
  int exit_seen = 0;
  for (;;) {
    int got = next_frame(&r, &f, 10000);
    if (got != 1) break;
    CHECK(!exit_seen);
    if (f.kind == K_EXIT) exit_seen = 1;
  }
  CHECK(exit_seen);
  CHECK(relay_exit(&r, 5000) == 3);
}

/* Arguments the relay refuses. */
static void refused(char **argv) {
  struct relay r;
  start_relay(&r, argv);
  CHECK(relay_exit(&r, 5000) == EXIT_PROTOCOL);
}

static void t_usage(void) {
  char *none[] = {"relay", NULL};
  char *no_agent[] = {"relay", "--forward", "8080", "--", NULL};
  char *zero[] = {"relay", "--forward", "0", "--", "true", NULL};
  char *high[] = {"relay", "--forward", "65536", "--", "true", NULL};
  char *word[] = {"relay", "--forward", "http", "--", "true", NULL};
  char *unknown[] = {"relay", "--bogus", "--", "true", NULL};
  char *probe_two[] = {"relay", "--forward", "1", "--forward", "2",  "--probe",
                       "a", "b", "c", "d", "e", "f", "g", NULL};
  char *probe_short[] = {"relay", "--forward", "1", "--probe", "a", "b", NULL};
  char ports[17][8];
  for (int i = 0; i < 17; i++)
    snprintf(ports[i], sizeof ports[i], "%d", 9000 + i);
  /* One more forward than MAX_FORWARDS. */
  char *too_many[40];
  int m = 0;
  too_many[m++] = "relay";
  for (int i = 0; i < 17; i++) {
    too_many[m++] = "--forward";
    too_many[m++] = ports[i];
  }
  too_many[m++] = "--";
  too_many[m++] = "true";
  too_many[m] = NULL;
  char **cases[] = {none, no_agent, zero,      high,      word,
                    unknown, probe_two, probe_short, too_many};
  for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) refused(cases[i]);
}

/* Main */

struct test {
  const char *name;
  void (*fn)(void);
};

static const struct test tests[] = {
    {"echo", t_echo},
    {"300 KB each way", t_large_echo},
    {"exit status", t_exit_status},
    {"killed by a signal", t_killed_by_signal},
    {"no such agent", t_no_such_agent},
    {"SIGTERM reaches the agent", t_term_passes_on},
    {"SIGINT reaches the agent", t_int_passes_on},
    {"no_new_privs", t_hardening},
    {"descriptors closed", t_fds_closed},
    {"two connections", t_multiplex},
    {"too many connections", t_too_many_connections},
    {"frame split across reads", t_split_frame},
    {"bad frame kind", t_bad_kind},
    {"frame too long", t_too_long},
    {"even channel", t_even_channel},
    {"exit frame from bactrian", t_exit_from_bactrian},
    {"open frame from bactrian", t_open_from_bactrian},
    {"input closed", t_input_closed},
    {"silence ends the run", t_liveness},
    {"heartbeat frames", t_frame_heartbeats},
    {"stuck frame holds the timer without a file",
     t_backpressure_without_file},
    {"stuck frame, heartbeat file", t_backpressure_with_file},
    {"frames do not count with a file", t_frames_do_not_count_with_file},
    {"heartbeat file removed", t_file_removed},
    {"silence with output queued", t_liveness_with_output_queued},
    {"exit flush is bounded", t_exit_flush_bounded},
    {"output backpressure", t_output_backpressure},
    {"exit frame is last", t_exit_is_last},
    {"arguments refused", t_usage},
};

/* Runs [fn] in a child of its own, with an alarm; whatever it leaves in its
   process group is killed after. 1 when it passed. */
static int run_test(void (*fn)(void)) {
  pid_t pid = fork();
  if (pid < 0) die("fork");
  if (pid == 0) {
    setpgid(0, 0);
    alarm(90);
    fn();
    fflush(stdout);
    _exit(failures == 0 ? 0 : 1);
  }
  int status;
  if (waitpid(pid, &status, 0) < 0) die("waitpid");
  kill(-pid, SIGKILL);
  return WIFEXITED(status) && WEXITSTATUS(status) == 0;
}

int main(int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr, "usage: test_relay_proc RELAY\n");
    return 2;
  }
  relay_path = argv[1];
  signal(SIGPIPE, SIG_IGN);
  int failed = 0;
  for (size_t i = 0; i < sizeof tests / sizeof tests[0]; i++) {
    int ok = run_test(tests[i].fn);
    printf("%s %s\n", ok ? "ok  " : "FAIL", tests[i].name);
    fflush(stdout);
    if (!ok) failed++;
  }
  printf("%d failed\n", failed);
  return failed == 0 ? 0 : 1;
}
