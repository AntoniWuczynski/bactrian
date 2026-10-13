// Toy bactrian agent (design §5.1): model calls through OPENAI_BASE_URL with a
// get_weather tool.
//
// Command mode (the default): one JSON object per line on stdin and stdout,
// the configuration in the process environment.
//
// HTTP mode (--http PORT, 0 for any free port): a server that prints its port
// as the first line of stdout. Each POST carries one message, the run in the
// X-Bactrian-Run header, and is answered with a JSON array of messages. The
// configuration of a run is the env of its start message, never the process
// environment, so runs in flight at once stay apart.
//
// When AGENT_MCP_COMMAND holds a JSON array (for example
// ["bactrian", "mcp-shim", "weather"]), get_weather runs on that stdio MCP
// server instead of returning a canned answer. AGENT_MCP_PROTOCOL picks the
// protocol revision: 2026-07-28 (the default) or 2025-11-25, which opens with
// initialize.
import { spawn } from "node:child_process";
import type { ChildProcessByStdio } from "node:child_process";
import { createServer } from "node:http";
import type { IncomingMessage, ServerResponse } from "node:http";
import { createInterface } from "node:readline";
import type { Interface } from "node:readline";
import type { Readable, Writable } from "node:stream";
import OpenAI from "openai";
import type {
  ChatCompletionMessageParam,
  ChatCompletionTool,
} from "openai/resources/chat/completions";

const WEATHER_RESULT = "18 degrees Celsius, cloudy";
const MAX_MODEL_CALLS = 5;
const DEFAULT_SYSTEM_PROMPT =
  "You are a concise assistant. Use the get_weather tool for weather questions.";

const TOOLS: ChatCompletionTool[] = [
  {
    type: "function",
    function: {
      name: "get_weather",
      description: "Get the current weather for a city.",
      parameters: {
        type: "object",
        properties: { city: { type: "string", description: "City name." } },
        required: ["city"],
        additionalProperties: false,
      },
    },
  },
];

// A string-valued environment: the process's or a start message's.
type Env = Record<string, string | undefined>;

type ToAgent =
  | { type: "start"; env: Env }
  | { type: "turn"; turn: number; content: string }
  | { type: "end" };

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}

function parseEnv(value: unknown): Env {
  if (!isRecord(value)) return {};
  const env: Env = {};
  for (const [key, entry] of Object.entries(value)) {
    if (typeof entry === "string") env[key] = entry;
  }
  return env;
}

function parseToAgent(text: string): ToAgent | undefined {
  let value: unknown;
  try {
    value = JSON.parse(text);
  } catch (error: unknown) {
    if (error instanceof SyntaxError) return undefined;
    throw error;
  }
  if (!isRecord(value)) return undefined;
  if (value["type"] === "start") return { type: "start", env: parseEnv(value["env"]) };
  if (value["type"] === "end") return { type: "end" };
  const { turn, content } = value;
  if (value["type"] === "turn" && typeof turn === "number" && typeof content === "string") {
    return { type: "turn", turn, content };
  }
  return undefined;
}

type Message = Record<string, string | number>;

function encode(message: Message): string {
  return JSON.stringify({ v: 1, ...message });
}

type Json = null | boolean | number | string | Json[] | { [key: string]: Json };

function isJson(value: unknown): value is Json {
  if (value === null) return true;
  if (Array.isArray(value)) return value.every(isJson);
  if (isRecord(value)) return Object.values(value).every(isJson);
  return typeof value === "boolean" || typeof value === "number" || typeof value === "string";
}

function parseJson(text: string): unknown {
  return JSON.parse(text);
}

function parseStringArray(text: string): string[] {
  const value = parseJson(text);
  if (!Array.isArray(value) || !value.every((x) => typeof x === "string")) {
    throw new Error("AGENT_MCP_COMMAND must be a JSON array of strings");
  }
  return value;
}

// The text items of a tools/call result.
function toolText(result: unknown): string {
  if (!isRecord(result) || !Array.isArray(result["content"])) {
    throw new Error("malformed tools/call result");
  }
  return result["content"]
    .filter(isRecord)
    .map((item) => (item["type"] === "text" && typeof item["text"] === "string" ? item["text"] : ""))
    .join("");
}

// A minimal MCP client for a stdio server: one JSON-RPC message per line, one
// request in flight at a time.
class McpClient {
  private readonly process: ChildProcessByStdio<Writable, Readable, null>;
  private readonly lines: AsyncIterator<string>;
  private readonly reader: Interface;
  private nextId = 0;

  private constructor(
    argv: string[],
    env: Env,
    private readonly protocol: string,
  ) {
    const [command, ...args] = argv;
    if (command === undefined) throw new Error("AGENT_MCP_COMMAND is empty");
    this.process = spawn(command, args, { stdio: ["pipe", "pipe", "inherit"], env });
    this.reader = createInterface({ input: this.process.stdout });
    this.lines = this.reader[Symbol.asyncIterator]();
  }

  static async start(argv: string[], env: Env, protocol: string): Promise<McpClient> {
    const mcp = new McpClient(argv, env, protocol);
    if (protocol === "2025-11-25") {
      await mcp.request("initialize", {
        protocolVersion: protocol,
        capabilities: {},
        clientInfo: { name: "toy-agent", version: "0" },
      });
      mcp.write({ jsonrpc: "2.0", method: "notifications/initialized" });
    }
    return mcp;
  }

  private write(message: Json): void {
    this.process.stdin.write(`${JSON.stringify(message)}\n`);
  }

  private async request(method: string, params: { [key: string]: Json }): Promise<unknown> {
    this.nextId += 1;
    const meta =
      this.protocol === "2025-11-25"
        ? {}
        : { _meta: { "io.modelcontextprotocol/protocolVersion": this.protocol } };
    this.write({ jsonrpc: "2.0", id: this.nextId, method, params: { ...params, ...meta } });
    const next = await this.lines.next();
    if (next.done === true) throw new Error("the MCP shim closed its output");
    const reply = parseJson(next.value);
    if (!isRecord(reply)) throw new Error("malformed MCP reply");
    const error = reply["error"];
    if (isRecord(error)) {
      throw new Error(`MCP error ${String(error["code"])}: ${String(error["message"])}`);
    }
    return reply["result"];
  }

  async callTool(name: string, args: { [key: string]: Json }): Promise<string> {
    return toolText(await this.request("tools/call", { name, arguments: args }));
  }

  close(): void {
    this.process.stdin.end();
    this.reader.close();
  }
}

// One conversation. The env is its whole configuration: the process
// environment in command mode, the start message's env in HTTP mode.
class Session {
  private readonly model: string;
  private readonly mcpCommand: string | undefined;
  private readonly mcpProtocol: string;
  private mcp: McpClient | undefined;
  private readonly client: OpenAI;
  private readonly history: ChatCompletionMessageParam[];

  constructor(private readonly env: Env) {
    this.model = env["AGENT_MODEL"] ?? "gpt-6-luna";
    this.mcpCommand = env["AGENT_MCP_COMMAND"];
    this.mcpProtocol = env["AGENT_MCP_PROTOCOL"] ?? "2026-07-28";
    this.client = new OpenAI({
      maxRetries: 0,
      apiKey: env["OPENAI_API_KEY"] ?? null,
      baseURL: env["OPENAI_BASE_URL"] ?? null,
    });
    this.history = [
      { role: "system", content: env["AGENT_SYSTEM_PROMPT"] ?? DEFAULT_SYSTEM_PROMPT },
    ];
  }

  private async weather(argumentsJson: string): Promise<string> {
    if (this.mcpCommand === undefined) return WEATHER_RESULT;
    // The shim finds bactrian through BACTRIAN_MCP_URL, which is in the run's
    // env and not necessarily in this process's.
    this.mcp ??= await McpClient.start(
      parseStringArray(this.mcpCommand),
      { ...process.env, ...this.env },
      this.mcpProtocol,
    );
    const args = parseJson(argumentsJson);
    if (!isRecord(args) || !isJson(args)) {
      throw new Error("tool arguments are not a JSON object");
    }
    return this.mcp.callTool("get_weather", args);
  }

  private async answer(content: string): Promise<string> {
    this.history.push({ role: "user", content });
    for (let call = 0; call < MAX_MODEL_CALLS; call++) {
      const completion = await this.client.chat.completions.create({
        model: this.model,
        messages: this.history,
        tools: TOOLS,
        reasoning_effort: "none",
      });
      const message = completion.choices[0]?.message;
      if (message === undefined) throw new Error("model returned no choices");
      this.history.push(message);
      const toolCalls = message.tool_calls ?? [];
      if (toolCalls.length === 0) return message.content ?? "";
      for (const toolCall of toolCalls) {
        this.history.push({
          role: "tool",
          tool_call_id: toolCall.id,
          content:
            toolCall.type === "function" && toolCall.function.name === "get_weather"
              ? await this.weather(toolCall.function.arguments)
              : "unknown tool",
        });
      }
    }
    throw new Error(`no final answer after ${MAX_MODEL_CALLS} model calls`);
  }

  async replyTo(turn: number, content: string): Promise<Message> {
    try {
      return { type: "reply", turn, content: await this.answer(content) };
    } catch (error: unknown) {
      const text = error instanceof Error ? error.message : String(error);
      return { type: "error", turn, message: text };
    }
  }

  close(): void {
    this.mcp?.close();
  }
}

async function runCommandMode(): Promise<void> {
  const session = new Session(process.env);
  const lines = createInterface({ input: process.stdin });
  for await (const line of lines) {
    if (line.trim() === "") continue;
    const message = parseToAgent(line);
    if (message === undefined) {
      process.stderr.write(`unparseable message: ${line}\n`);
      process.exitCode = 1;
      return;
    }
    if (message.type === "end") {
      session.close();
      return;
    }
    if (message.type === "start") continue;
    process.stdout.write(`${encode(await session.replyTo(message.turn, message.content))}\n`);
  }
}

const sessions = new Map<string, Session>();

// The status and the messages for one POST.
async function handleHttp(runId: string, body: string): Promise<[number, Message[]]> {
  const message = parseToAgent(body);
  if (message === undefined) return [400, []];
  if (message.type === "start") {
    sessions.set(runId, new Session(message.env));
    return [200, []];
  }
  const session = sessions.get(runId);
  if (session === undefined) return [404, []];
  if (message.type === "end") {
    sessions.delete(runId);
    session.close();
    return [200, []];
  }
  return [200, [await session.replyTo(message.turn, message.content)]];
}

async function readBody(request: IncomingMessage): Promise<string> {
  const chunks: Buffer[] = [];
  for await (const chunk of request) {
    if (Buffer.isBuffer(chunk)) chunks.push(chunk);
  }
  return Buffer.concat(chunks).toString("utf8");
}

async function respond(request: IncomingMessage, response: ServerResponse): Promise<void> {
  const header = request.headers["x-bactrian-run"];
  const [status, messages] = await handleHttp(
    typeof header === "string" ? header : "",
    await readBody(request),
  );
  response.writeHead(status, { "Content-Type": "application/json" });
  response.end(`[${messages.map(encode).join(",")}]`);
}

function runHttpMode(port: number): void {
  const server = createServer((request, response) => {
    respond(request, response).catch((error: unknown) => {
      process.stderr.write(`${error instanceof Error ? error.message : String(error)}\n`);
      response.writeHead(500).end();
    });
  });
  server.listen(port, "127.0.0.1", () => {
    const address = server.address();
    if (address !== null && typeof address === "object") {
      process.stdout.write(`${address.port}\n`);
    }
  });
}

const [flag, port] = process.argv.slice(2);
if (flag === "--http" && port !== undefined) {
  runHttpMode(Number(port));
} else {
  await runCommandMode();
}
