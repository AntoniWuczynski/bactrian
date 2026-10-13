// Toy bactrian agent (spec 5.1, command mode): one JSON object per line on
// stdin and stdout, model calls through OPENAI_BASE_URL with a get_weather tool.
import { createInterface } from "node:readline";
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
        content: toolCall.type === "function" && toolCall.function.name === "get_weather"
          ? WEATHER_RESULT
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
    if (message.type === "end") return;
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
