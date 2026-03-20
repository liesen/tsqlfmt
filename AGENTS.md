# AGENTS

This repo is a T-SQL formatter built on ScriptDOM and a `Doc` pipeline.

- Derive formatting from AST shape, token structure, or explicit layout intent.
- Do not inspect rendered `Doc` output to decide formatting.
- If a helper needs rendered text to choose a layout, change the helper interface.
- Prefer one general formatting routine per AST node shape over ad hoc special cases.
- Factor shared layout patterns into helpers; keep `tokenStreamDoc` as a fallback for unsupported nodes.
- For "collapse if it fits" decisions, prefer `Doc` alternatives that the pretty-printer can choose later. Avoid ad hoc width checks from rendered output or token streams when a reusable layout combinator would do better.
- Keep refactors small and reviewable.
- Preserve behavior unless the change is intentional and covered by tests.
- When a test fails, look for the missing structural formatter or shared helper; do not patch one case at a time.
- Treat tests as signals, not authority. If an expected output conflicts with these rules, fix the test or the shared formatter design instead of encoding the mismatch.
