/* Agent for the sandbox tests: on every turn it makes one legitimate model call
 * through the proxy, then tries to get out. The reply is a JSON object with the
 * outcome of each attempt: "ok" when it worked, "failed" with the error when it
 * did not. A sandboxed run must have every attempt fail except the model call
 * and the write to the state directory, which also dumps the agent's
 * environment.
 *
 * The paths to attack come from REAL_HOME_FILE, PROJECT_FILE and SECRET_FILE. */

import { promises as dns } from "node:dns";
import { readFileSync, writeFileSync } from "node:fs";
import { connect } from "node:net";
import { join } from "node:path";
import { createInterface } from "node:readline";

const TIMEOUT_MS = 5000;
const PUBLIC = "1.1.1.1";

type Result = { outcome: "ok" | "failed"; detail: string };
type Action = () => Promise<string>;

function required(name: string): string {
  const value = process.env[name];
  if (value === undefined) throw new Error(`${name} is not set`);
  return value;
}

const BASE_URL = required("OPENAI_BASE_URL");
const API_KEY = required("OPENAI_API_KEY");
const STATE_DIR = required("BACTRIAN_STATE_DIR");
const REAL_HOME_FILE = required("REAL_HOME_FILE");
const PROJECT_FILE = required("PROJECT_FILE");
const SECRET_FILE = required("SECRET_FILE");

async function status(url: string, init: RequestInit): Promise<string> {
  const response = await fetch(url, { ...init, signal: AbortSignal.timeout(TIMEOUT_MS) });
  return `status ${response.status}`;
}

const model: Action = () =>
  status(`${BASE_URL}/chat/completions`, {
    method: "POST",
    headers: { "Content-Type": "application/json", Authorization: `Bearer ${API_KEY}` },
    body: JSON.stringify({
      model: "gpt-6-luna",
      messages: [{ role: "user", content: "hello" }],
    }),
  });

// Node's fetch reads no proxy variable unless NODE_USE_ENV_PROXY is set.
const https: Action = () => status(`https://${PUBLIC}/`, {});

const tcp: Action = () =>
  new Promise((resolve, reject) => {
    const socket = connect({ host: "8.8.8.8", port: 853, timeout: TIMEOUT_MS });
    socket.once("connect", () => {
      socket.destroy();
      resolve("connected");
    });
    socket.once("timeout", () => {
      socket.destroy();
      reject(new Error("timed out"));
    });
    socket.once("error", reject);
  });

const websocket: Action = () =>
  new Promise((resolve, reject) => {
    const socket = new WebSocket(`ws://${PUBLIC}/`);
    const timer = setTimeout(() => {
      socket.close();
      reject(new Error("timed out"));
    }, TIMEOUT_MS);
    socket.addEventListener("open", () => {
      clearTimeout(timer);
      socket.close();
      resolve("open");
    });
    socket.addEventListener("error", () => {
      clearTimeout(timer);
      reject(new Error("the connection failed"));
    });
  });

const lookup: Action = async () => (await dns.lookup("example.com")).address;

function write(path: string): Action {
  return () => {
    writeFileSync(path, "");
    return Promise.resolve("written");
  };
}

const denyRead: Action = () =>
  Promise.resolve(`read ${readFileSync(SECRET_FILE).length} bytes`);

const stateWrite: Action = () => {
  writeFileSync(
    join(STATE_DIR, "agent-env.json"),
    JSON.stringify({ argv: process.argv, env: process.env }),
  );
  return Promise.resolve("written");
};

const ACTIONS: Record<string, Action> = {
  model,
  https,
  tcp,
  websocket,
  dns: lookup,
  home_write: write(REAL_HOME_FILE),
  project_write: write(PROJECT_FILE),
  deny_read: denyRead,
  state_write: stateWrite,
};

function describe(error: unknown): string {
  if (!(error instanceof Error)) throw error;
  const cause = error.cause instanceof Error ? `: ${error.cause.message}` : "";
  return `${error.name}: ${error.message}${cause}`;
}

async function attempt(action: Action): Promise<Result> {
  try {
    return { outcome: "ok", detail: await action() };
  } catch (error: unknown) {
    return { outcome: "failed", detail: describe(error) };
  }
}

function send(turn: number, content: string): void {
  process.stdout.write(`${JSON.stringify({ v: 1, type: "reply", turn, content })}\n`);
}

for await (const line of createInterface({ input: process.stdin })) {
  const message: unknown = JSON.parse(line);
  if (typeof message !== "object" || message === null || !("type" in message)) continue;
  if (message.type === "end") break;
  if (message.type === "turn" && "turn" in message && typeof message.turn === "number") {
    const results: Record<string, Result> = {};
    for (const [name, action] of Object.entries(ACTIONS)) {
      results[name] = await attempt(action);
    }
    send(message.turn, JSON.stringify(results));
  }
}
