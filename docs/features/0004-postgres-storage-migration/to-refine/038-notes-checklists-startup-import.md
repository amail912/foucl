# 038 Notes And Checklists Startup Import

## Objective

Add startup filesystem-to-Postgres bootstrap import for notes and checklists when those domains are switched to Postgres.

## Scope

- Run import only when notes and/or checklists backend is `postgres`.
- Read filesystem domain data and existing Postgres domain data at startup.
- Use deterministic conflict policy (`postgres-wins`) for overlapping keys.
- Emit one domain overlap warning when both sources contain data.
- Emit conflict warnings with domain/key context for skipped filesystem conflicts.
- Keep import idempotent and safe across repeated startup runs.

## Dependencies And Boundaries

Depends on:

- 033 Notes And Checklists Backend Selection And Wiring.
- 035 Notes And Checklists Postgres Schema And Migrations.
- 036 Notes And Checklists Postgres Adapter Implementation.

Out of scope:

- Auth/session startup import behavior.
- Calendar/trip-sharing startup import behavior.
- Non-startup migration tooling.

## Acceptance Criteria

1. Notes/checklists imports are gated by domain backend flags.
2. Overlap warnings and conflict warnings are emitted with domain/key context.
3. Filesystem-only records are imported deterministically.
4. Postgres-conflicting records preserve Postgres values.
5. Repeated startup runs preserve stable end state without incorrect duplicates.
6. No notes/checklists HTTP contract drift is introduced.
