let css =
  {|
:root {
  color-scheme: light dark;
  --bg: #faf9f6;
  --card: #ffffff;
  --fg: #1d1c1a;
  --muted: #5c5952;
  --line: #dcd8cf;
  --bad: #b3261e;
  --good: #1a7340;
  --warn: #85560a;
  --info: #2a5db0;
  --band: #e6ecf8;
  --bad-bg: #fbe9e7;
  --good-bg: #e4f3ea;
  --warn-bg: #fbf0d9;
  --info-bg: #e8eefa;
}
@media (prefers-color-scheme: dark) {
  :root {
    --bg: #15140f;
    --card: #1e1d17;
    --fg: #ece9e1;
    --muted: #a7a398;
    --line: #3a382f;
    --bad: #ff8f85;
    --good: #72d19b;
    --warn: #e5b758;
    --info: #8db5fa;
    --band: #243049;
    --bad-bg: #3a1f1c;
    --good-bg: #1b3326;
    --warn-bg: #382c12;
    --info-bg: #1d2740;
  }
}
* { box-sizing: border-box; }
html { -webkit-text-size-adjust: 100%; }
body {
  margin: 0;
  background: var(--bg);
  color: var(--fg);
  font: 16px/1.5 system-ui, -apple-system, Segoe UI, Roboto, sans-serif;
}
main { max-width: 60rem; margin: 0 auto; padding: 1.25rem 16px 4rem; }
h1 { font-size: 1.6rem; line-height: 1.2; margin: 0 0 0.25rem; }
h2 { font-size: 1.2rem; margin: 2.5rem 0 0.5rem; border-bottom: 1px solid var(--line); padding-bottom: 0.25rem; }
h3 { font-size: 1rem; margin: 1.25rem 0 0.25rem; }
p { margin: 0.5rem 0; }
.sub { color: var(--muted); margin-top: 0; }
.note { color: var(--muted); font-size: 0.9rem; }
.warning { color: var(--warn); background: var(--warn-bg); padding: 0.5rem 0.75rem; font-weight: 600; }
dl.facts { display: grid; grid-template-columns: max-content 1fr; gap: 0.25rem 1rem; margin: 1rem 0; }
dl.facts dt { color: var(--muted); }
dl.facts dd { margin: 0; }
.scroll { overflow-x: auto; margin: 0 0 1rem; }
.caption { color: var(--muted); font-size: 0.85rem; margin: 0.75rem 0 0.25rem; }
.sr-only { position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px; overflow: hidden; clip: rect(0, 0, 0, 0); white-space: nowrap; border: 0; }
table { border-collapse: collapse; width: 100%; font-size: 0.9rem; font-variant-numeric: tabular-nums; }
th, td { text-align: left; padding: 0.4rem 0.6rem; border-bottom: 1px solid var(--line); vertical-align: top; }
thead th { border-bottom: 2px solid var(--line); white-space: nowrap; }
td.num { text-align: right; white-space: nowrap; }
code, pre { font: 0.85rem/1.4 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; }
pre { background: var(--card); border: 1px solid var(--line); border-radius: 6px; padding: 0.75rem; overflow-x: auto; margin: 0.5rem 0; }
details { background: var(--card); border: 1px solid var(--line); border-radius: 6px; padding: 0.4rem 0.75rem; margin: 0.5rem 0; }
details[open] { padding-bottom: 0.75rem; }
summary { cursor: pointer; padding: 0.25rem 0; }
.badge { display: inline-block; padding: 0.05rem 0.5rem; border-radius: 999px; border: 1px solid currentColor; font-size: 0.8rem; font-weight: 600; white-space: nowrap; }
.regression { color: var(--bad); }
.improvement { color: var(--good); }
.no-change { color: var(--info); }
.inconclusive { color: var(--warn); }
.badge.regression { background: var(--bad-bg); }
.badge.improvement { background: var(--good-bg); }
.badge.no-change { background: var(--info-bg); }
.badge.inconclusive { background: var(--warn-bg); }
.badge.new { color: var(--warn); background: var(--warn-bg); }
.badge.plain { color: var(--muted); }
figure { margin: 0.5rem 0 1rem; }
figcaption { color: var(--muted); font-size: 0.85rem; }
svg.chart { display: block; width: 100%; max-width: 32rem; height: auto; }
svg.chart text { font: 12px system-ui, sans-serif; fill: var(--fg); }
svg.chart text.axis, svg.chart text.val { fill: var(--muted); }
svg.chart .strong text.lbl { font-weight: 700; }
svg.chart .band { fill: var(--band); }
svg.chart .zero { stroke: var(--fg); stroke-width: 1; stroke-dasharray: 3 3; }
svg.chart .axisline, svg.chart .tick { stroke: var(--muted); stroke-width: 1; }
svg.chart .ci { stroke: var(--muted); stroke-width: 2; fill: none; }
svg.chart .ci.regression { stroke: var(--bad); }
svg.chart .ci.improvement { stroke: var(--good); }
svg.chart .ci.no-change { stroke: var(--info); }
svg.chart .ci.inconclusive { stroke: var(--warn); }
svg.chart circle.pt { fill: var(--card); stroke-width: 2.5; }
svg.chart .bar.base { fill: var(--muted); }
svg.chart .bar.cand { fill: var(--info); }
svg.chart .whisker { stroke: var(--fg); stroke-width: 1.5; }
ul.messages { margin: 0.25rem 0; padding-left: 1.25rem; }
@media (max-width: 36rem) {
  dl.facts { grid-template-columns: 1fr; gap: 0; }
  dl.facts dd { margin-bottom: 0.5rem; }
  h1 { font-size: 1.35rem; }
}
|}
