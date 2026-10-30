/* Unit tests of vm/relay.c: its buffers, frames and delivery, called directly.
   The relay is Linux-only (prctl, accept4), so dune builds this on Linux
   alone. Its source is included with main renamed, so the tests reach its
   static functions. Each test runs in a process of its own, because the relay
   keeps its state in globals, with an alarm. The relay as a whole is tested
   from outside in test_relay_proc.c. */

#define main relay_main
#include "../../vm/relay.c"
#undef main

#include <sys/stat.h>

static int failures;

#define CHECK(cond)                                                   \
  do {                                                                \
    if (!(cond)) {                                                    \
      fprintf(stderr, "%s:%d: check failed: %s\n", __FILE__, __LINE__, \
              #cond);                                                 \
      failures++;                                                     \
    }                                                                 \
  } while (0)

/* Tests */

typedef void (*test_fn)(void);

struct test {
  const char *name;
  test_fn fn;
};

/* What a child test printed on its standard output, and how it ended. */
struct outcome {
  int status;
  unsigned char out[1 << 16];
  size_t out_len;
};

/* Runs [fn] in a child whose standard output is captured. A child that ends
   with failures exits 1. */
static void run_captured(test_fn fn, struct outcome *o) {
  int p[2];
  if (pipe(p) < 0) die("pipe");
  pid_t pid = fork();
  if (pid < 0) die("fork");
  if (pid == 0) {
    setpgid(0, 0);
    alarm(60);
    dup2(p[1], 1);
    close(p[0]);
    close(p[1]);
    fn();
    fflush(stdout);
    _exit(failures == 0 ? 0 : 1);
  }
  close(p[1]);
  o->out_len = 0;
  for (;;) {
    ssize_t n = read(p[0], o->out + o->out_len, sizeof o->out - o->out_len);
    if (n <= 0) break;
    o->out_len += (size_t)n;
  }
  close(p[0]);
  if (waitpid(pid, &o->status, 0) < 0) die("waitpid");
  kill(-pid, SIGKILL);
}

static int exited_with(const struct outcome *o, int code) {
  return WIFEXITED(o->status) && WEXITSTATUS(o->status) == code;
}

static void ensure_state(void) {
  buf_init(&out, OUT_CAP);
  buf_init(&to_agent, SINK_CAP);
  for (int k = 0; k < MAX_CONNS; k++) {
    conns[k].fd = -1;
    buf_init(&conns[k].sink, SINK_CAP);
  }
}

static void set_frame(uint32_t channel, enum kind k, const void *payload,
                      size_t n) {
  put_u32(in_frame, channel);
  in_frame[4] = (unsigned char)k;
  put_u32(in_frame + 5, (uint32_t)n);
  if (n > 0) memcpy(in_frame + HEADER, payload, n);
  in_have = HEADER + n;
}

/* Unit tests */

static void t_u32(void) {
  unsigned char b[4];
  uint32_t samples[] = {0, 1, 255, 256, 65536, 0xdeadbeefu, 0xffffffffu};
  for (size_t i = 0; i < sizeof samples / sizeof samples[0]; i++) {
    put_u32(b, samples[i]);
    CHECK(get_u32(b) == samples[i]);
  }
  put_u32(b, 0x01020304u);
  CHECK(b[0] == 1 && b[1] == 2 && b[2] == 3 && b[3] == 4);
}

static void t_buf(void) {
  struct buf b;
  buf_init(&b, 8);
  CHECK(buf_room(&b) == 8 && buf_used(&b) == 0);
  buf_put(&b, "abcdef", 6);
  CHECK(buf_used(&b) == 6 && buf_room(&b) == 2);
  buf_take(&b, 4);
  CHECK(buf_used(&b) == 2 && buf_room(&b) == 6);
  /* Not enough room after len, so the bytes move to the front. */
  buf_put(&b, "ghijkl", 6);
  CHECK(buf_used(&b) == 8 && buf_room(&b) == 0);
  CHECK(memcmp(b.data + b.off, "efghijkl", 8) == 0);
  buf_take(&b, 8);
  CHECK(buf_used(&b) == 0 && b.off == 0 && b.len == 0);
}

static void t_frame_layout(void) {
  ensure_state();
  frame(0x01020304u, K_OPEN, "xy", 2);
  const unsigned char want[] = {1, 2, 3, 4, K_OPEN, 0, 0, 0, 2, 'x', 'y'};
  CHECK(buf_used(&out) == sizeof want);
  CHECK(memcmp(out.data + out.off, want, sizeof want) == 0);
  buf_take(&out, sizeof want);
  frame(7, K_EOF, NULL, 0);
  CHECK(buf_used(&out) == HEADER);
  CHECK(get_u32(out.data + out.off) == 7 && out.data[out.off + 4] == K_EOF &&
        get_u32(out.data + out.off + 5) == 0);
}

static void t_out_room(void) {
  ensure_state();
  CHECK(out_has_room(CHUNK));
  CHECK(out_has_room(OUT_CAP - HEADER - RESERVE));
  CHECK(!out_has_room(OUT_CAP - HEADER - RESERVE + 1));
  /* Fill all but the reserve: data stops, control frames still fit. */
  unsigned char block[1024] = {0};
  while (buf_room(&out) > RESERVE + HEADER + CHUNK) {
    size_t n = sizeof block;
    if (n > buf_room(&out) - RESERVE - HEADER - CHUNK)
      n = buf_room(&out) - RESERVE - HEADER - CHUNK;
    buf_put(&out, block, n);
  }
  buf_put(&out, block, 1);
  CHECK(!out_has_room(CHUNK));
  frame(1, K_CLOSE, NULL, 0);
  CHECK(buf_room(&out) < OUT_CAP);
}

static void t_json_string(void) {
  char dst[64];
  json_string(dst, sizeof dst, "a\"b\\c");
  CHECK(strcmp(dst, "a\\\"b\\\\c") == 0);
  json_string(dst, sizeof dst, "tab\there");
  CHECK(strcmp(dst, "tab\\u0009here") == 0);
  json_string(dst, sizeof dst, "caf\xc3\xa9");
  CHECK(strcmp(dst, "caf\\u00c3\\u00a9") == 0);
  /* Cut to fit, always terminated, never past the buffer. */
  char small[10];
  json_string(small, sizeof small, "0123456789abcdef");
  CHECK(strlen(small) < sizeof small);
  CHECK(strncmp(small, "012", 3) == 0);
}

static void t_log_failure(void) {
  ensure_state();
  log_failure(8080, "relay", "went \"wrong\"");
  CHECK(buf_used(&out) > HEADER);
  const unsigned char *f = out.data + out.off;
  CHECK(get_u32(f) == 0 && f[4] == K_LOG);
  size_t n = get_u32(f + 5);
  CHECK(buf_used(&out) == HEADER + n);
  const char *want =
      "{\"kind\":\"relay_failed\",\"port\":8080,\"socket\":\"relay\","
      "\"reason\":\"went \\\"wrong\\\"\"}";
  CHECK(n == strlen(want) && memcmp(f + HEADER, want, n) == 0);
  /* A log line that does not fit is dropped, not queued halfway. */
  size_t before = buf_used(&out);
  struct buf full = out;
  out.cap = out.len;
  log_failure(1, "relay", "dropped");
  CHECK(buf_used(&out) == before);
  out = full;
}

static void t_in_needed(void) {
  in_have = 0;
  CHECK(in_needed() == HEADER);
  set_frame(1, K_DATA, "abc", 3);
  in_have = HEADER;
  CHECK(in_needed() == HEADER + 3);
  in_have = HEADER - 1;
  CHECK(in_needed() == HEADER);
}

static void t_deliver_channel0(void) {
  ensure_state();
  agent_in = 5; /* any open descriptor number: deliver only queues */
  set_frame(0, K_HEARTBEAT, NULL, 0);
  CHECK(deliver() == 1);
  set_frame(0, K_DATA, "hello", 5);
  CHECK(deliver() == 1);
  CHECK(buf_used(&to_agent) == 5 &&
        memcmp(to_agent.data + to_agent.off, "hello", 5) == 0);
  /* No room: the frame is kept for later. */
  unsigned char block[SINK_CAP] = {0};
  buf_take(&to_agent, 5);
  buf_put(&to_agent, block, SINK_CAP - 3);
  set_frame(0, K_DATA, "hello", 5);
  CHECK(deliver() == 0);
  CHECK(buf_used(&to_agent) == SINK_CAP - 3);
  /* Eof with data still queued waits for the queue to drain. */
  set_frame(0, K_EOF, NULL, 0);
  CHECK(deliver() == 1);
  CHECK(agent_in_eof == 1 && agent_in == 5);
  /* Data after eof is ignored. */
  set_frame(0, K_DATA, "x", 1);
  CHECK(deliver() == 1);
  CHECK(buf_used(&to_agent) == SINK_CAP - 3);
}

static void t_deliver_unknown_channel(void) {
  ensure_state();
  set_frame(9, K_DATA, "x", 1);
  CHECK(deliver() == 1);
  set_frame(9, K_EOF, NULL, 0);
  CHECK(deliver() == 1);
  set_frame(9, K_CLOSE, NULL, 0);
  CHECK(deliver() == 1);
}

static void t_deliver_even_channel(void) {
  ensure_state();
  set_frame(2, K_DATA, "x", 1);
  deliver();
  _exit(0); /* not reached: protocol_error exits 70 */
}

static void t_deliver_bad_kind_0(void) {
  ensure_state();
  set_frame(0, K_OPEN, NULL, 0);
  deliver();
  _exit(0);
}

static void t_deliver_bad_kind_conn(void) {
  ensure_state();
  set_frame(1, K_EXIT, NULL, 0);
  deliver();
  _exit(0);
}

static void t_deliver_conn(void) {
  ensure_state();
  int sp[2];
  CHECK(socketpair(AF_UNIX, SOCK_STREAM, 0, sp) == 0);
  conns[0].fd = sp[0];
  conns[0].channel = 5;
  conns[0].eof_in = conns[0].shut = conns[0].eof_out = 0;
  set_frame(5, K_DATA, "to-conn", 7);
  CHECK(deliver() == 1);
  CHECK(buf_used(&conns[0].sink) == 7);
  write_conn(&conns[0]);
  char got[16];
  CHECK(read(sp[1], got, sizeof got) == 7 && memcmp(got, "to-conn", 7) == 0);
  /* Eof shuts down writing once the sink is empty. */
  set_frame(5, K_EOF, NULL, 0);
  CHECK(deliver() == 1);
  CHECK(conns[0].shut == 1);
  CHECK(read(sp[1], got, sizeof got) == 0);
  /* A close frame frees the slot without telling bactrian back. */
  size_t before = buf_used(&out);
  set_frame(5, K_CLOSE, NULL, 0);
  CHECK(deliver() == 1);
  CHECK(conns[0].fd == -1 && buf_used(&out) == before);
}

static void t_conn_finish(void) {
  ensure_state();
  int sp[2];
  CHECK(socketpair(AF_UNIX, SOCK_STREAM, 0, sp) == 0);
  set_nonblock(sp[0]);
  conns[0].fd = sp[0];
  conns[0].channel = 3;
  conns[0].eof_in = conns[0].shut = conns[0].eof_out = 0;
  CHECK(write(sp[1], "abc", 3) == 3);
  read_conn(&conns[0]);
  CHECK(buf_used(&out) == HEADER + 3);
  buf_take(&out, HEADER + 3);
  close(sp[1]);
  read_conn(&conns[0]); /* end of file: eof out */
  CHECK(conns[0].eof_out == 1 && conns[0].fd >= 0);
  CHECK(buf_used(&out) == HEADER && out.data[out.off + 4] == K_EOF);
  buf_take(&out, HEADER);
  /* Both directions done: closed, and bactrian told. */
  set_frame(3, K_EOF, NULL, 0);
  conns[0].eof_in = 1;
  write_conn(&conns[0]);
  CHECK(conns[0].fd == -1);
  CHECK(buf_used(&out) == HEADER && out.data[out.off + 4] == K_CLOSE);
}

/* Main */

static const struct test tests[] = {
    {"u32 round trip", t_u32},
    {"buffer", t_buf},
    {"frame layout", t_frame_layout},
    {"room for data keeps a reserve", t_out_room},
    {"json strings", t_json_string},
    {"relay failure log line", t_log_failure},
    {"frame length needed", t_in_needed},
    {"deliver on channel 0", t_deliver_channel0},
    {"deliver to an unknown channel", t_deliver_unknown_channel},
    {"deliver to a connection", t_deliver_conn},
    {"connection finishes both ways", t_conn_finish},
};

/* These end the child on purpose, with 70. */
static const struct test dies[] = {
    {"deliver on an even channel", t_deliver_even_channel},
    {"deliver a bad kind on channel 0", t_deliver_bad_kind_0},
    {"deliver a bad kind on a connection", t_deliver_bad_kind_conn},
};

int main(void) {
  int failed = 0;
  signal(SIGPIPE, SIG_IGN); /* as the relay does */
  for (size_t i = 0; i < sizeof tests / sizeof tests[0]; i++) {
    struct outcome *o = malloc(sizeof *o);
    run_captured(tests[i].fn, o);
    int ok = exited_with(o, 0);
    printf("%s %s\n", ok ? "ok  " : "FAIL", tests[i].name);
    if (!ok) failed++;
    free(o);
  }
  for (size_t i = 0; i < sizeof dies / sizeof dies[0]; i++) {
    struct outcome *o = malloc(sizeof *o);
    run_captured(dies[i].fn, o);
    int ok = exited_with(o, EXIT_PROTOCOL);
    printf("%s %s\n", ok ? "ok  " : "FAIL", dies[i].name);
    if (!ok) failed++;
    free(o);
  }
  printf("%d failed\n", failed);
  return failed == 0 ? 0 : 1;
}
