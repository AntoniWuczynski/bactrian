// Toy bactrian agent (spec 5.1, command mode): one JSON object per line on
// stdin and stdout, model calls through OPENAI_BASE_URL with a get_weather tool.
//
// When AGENT_MCP_COMMAND holds a JSON array (for example
// ["bactrian", "mcp-shim", "weather"]), get_weather runs on that stdio MCP
// server instead of returning a canned answer. AGENT_MCP_PROTOCOL picks the
// protocol revision: 2026-07-28 (the default) or 2025-11-25, which opens with
// initialize.
import { spawn } from "node:child_process";
import type { ChildProcessByStdio } from "node:child_process";
import { createInterface } from "node:readline";
import type { Interface } from "node:readline";
import type { Readable, Writable } from "node:stream";
import OpenAI from "openai";
import type {
  ChatCompletionMessageParam,
  ChatCompletionTool,
} from "openai/resources/chat/completions";

const MODEL = process.env["AGENT_MODEL"] ?? "gpt-6-luna";
const WEATHER_RESULT = "18 degrees Celsius, cloudy";
const MAX_MODEL_CALLS = 5;
const SYSTEM_PROMPT =
  process.env["AGENT_SYSTEM_PROMPT"] ??
  "You are a concise assistant. Use the get_weather tool for weather questions.";

const MCP_COMMAND = process.env["AGENT_MCP_COMMAND"];
const MCP_PROTOCOL = process.env["AGENT_MCP_PROTOCOL"] ?? "2026-07-28";

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

type ToAgent =
  | { type: "start" }
  | { type: "turn"; turn: number; content: string }
  | { type: "end" };

function parseToAgent(line: string): ToAgent | undefined {
  let value: unknown;
  try {
    value = JSON.parse(line);
  } catch (error: unknown) {
    if (error instanceof SyntaxError) return undefined;
    throw error;
  }
  if (typeof value !== "object" || value === null || !("type" in value)) {
    return undefined;
  }
  if (value.type === "start") return { type: "start" };
  if (value.type === "end") return { type: "end" };
  if (
    value.type === "turn" &&
    "turn" in value &&
    typeof value.turn === "number" &&
    "content" in value &&
    typeof value.content === "string"
  ) {
    return { type: "turn", turn: value.turn, content: value.content };
  }
  return undefined;
}

function send(message: Record<string, string | number>): void {
  process.stdout.write(`${JSON.stringify({ v: 1, ...message })}\n`);
}

type Json = null | boolean | number | string | Json[] | { [key: string]: Json };

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}

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

  private constructor(argv: string[]) {
    const [command, ...args] = argv;
    if (command === undefined) throw new Error("AGENT_MCP_COMMAND is empty");
    this.process = spawn(command, args, { stdio: ["pipe", "pipe", "inherit"] });
    this.reader = createInterface({ input: this.process.stdout });
    this.lines = this.reader[Symbol.asyncIterator]();
  }

  static async start(argv: string[]): Promise<McpClient> {
    const mcp = new McpClient(argv);
    if (MCP_PROTOCOL === "2025-11-25") {
      await mcp.request("initialize", {
        protocolVersion: MCP_PROTOCOL,
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
      MCP_PROTOCOL === "2025-11-25"
        ? {}
        : { _meta: { "io.modelcontextprotocol/protocolVersion": MCP_PROTOCOL } };
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

let mcp: McpClient | undefined;

async function weather(argumentsJson: string): Promise<string> {
  if (MCP_COMMAND === undefined) return WEATHER_RESULT;
  mcp ??= await McpClient.start(parseStringArray(MCP_COMMAND));
  const args = parseJson(argumentsJson);
  if (!isRecord(args) || !isJson(args)) {
    throw new Error("tool arguments are not a JSON object");
  }
  return mcp.callTool("get_weather", args);
}

const client = new OpenAI({ maxRetries: 0 });
const history: ChatCompletionMessageParam[] = [
  {
    role: "system",
    content: SYSTEM_PROMPT,
  },
];

async function answer(content: string): Promise<string> {
  history.push({ role: "user", content });
  for (let call = 0; call < MAX_MODEL_CALLS; call++) {
    const completion = await client.chat.completions.create({
      model: MODEL,
      messages: history,
      tools: TOOLS,
      reasoning_effort: "none",
    });
    const message = completion.choices[0]?.message;
    if (message === undefined) throw new Error("model returned no choices");
    history.push(message);
    const toolCalls = message.tool_calls ?? [];
    if (toolCalls.length === 0) return message.content ?? "";
    for (const toolCall of toolCalls) {
      history.push({
        role: "tool",
        tool_call_id: toolCall.id,
        content:
          toolCall.type === "function" && toolCall.function.name === "get_weather"
            ? await weather(toolCall.function.arguments)
            : "unknown tool",
      });
    }
  }
  throw new Error(`no final answer after ${MAX_MODEL_CALLS} model calls`);
}

async function main(): Promise<void> {
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
      mcp?.close();
      return;
    }
    if (message.type === "start") continue;
    try {
      send({ type: "reply", turn: message.turn, content: await answer(message.content) });
    } catch (error: unknown) {
      const text = error instanceof Error ? error.message : String(error);
      send({ type: "error", turn: message.turn, message: text });
    }
  }
}

await main();
