# AGENTS

T-SQL formatter built on ScriptDOM and a `Doc` pipeline.

External interface target is SQL Prompt compatibility: style files follow the SQL Prompt shape, and the CLI follows SQL Prompt's `formatSql` command model.

Primary goal: reproduce the style in `default-style.json`.

## Rules

- Prefer parity with `default-style.json` over broad style coverage.
- Do not add support for style modes we do not use unless the change clearly requires it.
- Derive formatting from AST shape, token structure, or explicit layout intent.
- Never inspect rendered `Doc` output to choose a layout. If a helper needs rendered text, redesign the helper.
- Prefer structural `Doc` alternatives (`group`, `softline`, `line`, `nest`) over ad hoc width checks.
- Prefer shared formatters and helpers over node-by-node special cases.
- Use `tokenStreamDoc` for unsupported constructs.
- Keep changes small. Preserve behavior unless the change is intentional and tested.
- When tests fail, look for a missing shared formatter or helper, not a one-off patch.
- For touched `.fs` files: build, run Fantomas, then run tests.

## Modules

- `src/tsqlfmt/Doc.fs`: pretty-printing algebra and renderer.
- `src/tsqlfmt/Formatter.fs`: AST-to-`Doc` formatter and top-level `format` entry point.
- `src/tsqlfmt/Style.fs`: SQL Prompt-compatible style model, defaults, JSON loading, and validation.
- `src/tsqlfmt/Lists.fs`: shared list layout, comma decoration, and anchoring helpers.
- `src/tsqlfmt/Parenthesis.fs`: parenthesis combinators grouped by style concept.
- `src/tsqlfmt/FunctionCalls.fs`: function-call assembly helpers.
- `src/tsqlfmt/Keywords.fs`: keyword classification and casing.
- `src/tsqlfmt/Identifiers.fs`: identifier and multipart-name rendering.
- `src/tsqlfmt/Trivia.fs`: comment and token-trivia helpers.
- `src/tsqlfmt/CliArgs.fs`: SQL Prompt-compatible CLI parsing.
- `src/tsqlfmt/Program.fs`: executable entry point.
- `tests/tsqlfmt.Tests/TestSupport.fs`: shared test helpers.
- `tests/tsqlfmt.Tests/*Tests.fs`: behavior tests by formatter area.

## `Doc` Limits

We are not trying to make `Doc` column-aware or alignment-heavy right now.

The styles we currently care about do not require the unsupported modes below.

- Supports: structural indentation, flat-vs-broken alternatives, anchored layouts, and "collapse if it fits" when both layouts are encoded structurally.
- Anchored helpers are our structural approximation of hanging indentation: they derive the anchor from AST/layout structure instead of the current rendered column.
- Does not support: right-aligned modes, measuring rendered text to align later tokens, tabular alignment, or layout decisions based on rendered output.
- Policy: do not hack around these limits. Reject unsupported right-aligned style settings in `Style.validateStyle`. If a construct cannot be formatted structurally, preserve tokens instead of adding a fragile special case.
