# Cold Summary Markdown Isolation

## Scope

Preserve the existing tool-summary fence isolation when completed tools move
outside the hot tail. No changes to parser reclamation, canonical messages,
tool payloads, or user-visible summary text.

## Diagnosis

The visible Session Output contained a cold bash summary with an unescaped
opening fence. The Markdown host parser consumed subsequent assistant text,
interpreting a legitimate closing fence as another opening fence. This broke
headings, emphasis, and tables through the end of the transcript.

The earlier fix in `15dda1a` escaped fences during summary insertion, but
`pi-coding-agent--cool-tool-overlay` reinserted the original summary string
directly. A synthetic hot-to-cold transition reproduced the same failure:
the table following the tool changed from one detected table to zero.

## Implementation

The cold-summary branch now calls the existing summary-fragment insertion
helper. Non-summary cold tool bodies keep their existing fenced rendering.

## Verification

- New ERT regression fails before the change and passes afterward.
- Covers backtick, tilde, and indented fences, unchanged visible text,
  removed buttons, following headings, genuine code blocks, and table overlays.
- `make test`: 1548 passed, 1 version-dependent md-ts skip, 0 unexpected.
- `make compile` and `git diff --check`: passed.
- Local live Output repaired without rebuilding its incomplete canonical cache.
  Session files remain unchanged. Desktop screenshot confirms restored grid
  and prose styling; user confirmation remains pending.
- Repository and Straight build bytecode rebuilt; changed function hot-loaded.

## Delivery

The user requested Git submission. Package publication and the parent Straight
lock update follow the repository's multi-machine delivery convention.

## Remaining

- User verification while scrolling history and receiving subsequent output.
- Remove temporary runtime diagnostic advice after confirmation.
- Other machines have not been updated.
