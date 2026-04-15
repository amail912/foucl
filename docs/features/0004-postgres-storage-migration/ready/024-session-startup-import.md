# 024 Session Startup Import

## Objective

Add startup filesystem-to-Postgres bootstrap import for the session domain when sessions are switched to Postgres.

## Scope

- Run session import only when `sessionBackend=postgres`.
- Read filesystem session records (handles, states, user-state bindings) at startup.
- Read existing Postgres session records at startup.
- If both stores contain session data, log a warning with session domain context.
- Import session records into Postgres deterministically with explicit conflict handling.
- Keep import idempotent and safe on repeated startup runs.

## Import Conflict Policy

- `session_states` conflict key: `state_id`.
- `session_handles` conflict key: `session_id`.
- `session_user_bindings` conflict key: `user_id`.
- Filesystem-only record -> insert/upsert into Postgres.
- Postgres-only record -> keep Postgres record unchanged.
- Record exists in both stores for same conflict key -> Postgres record is authoritative; skip filesystem conflicting record and emit warning with domain/key context.

## Import Execution Contract

- Import executes during startup only.
- Import ordering must satisfy schema relations from 022:
  - import `session_states` first,
  - then `session_handles`,
  - then `session_user_bindings`.
- Conflict decisions are deterministic (`postgres-wins` for duplicate key collisions).
- Import path must not modify HTTP-visible session/cookie behavior.

## Dependencies And Boundaries

- Depends on:
  - 018 Session Backend Selection And Wiring
  - 022 Session Postgres Schema And Migrations
  - 020 Session Postgres Adapter Implementation

Out of scope:

- Auth startup import behavior (023).
- Calendar/trip-sharing/notes/checklists import.
- Non-startup migration tooling.

## Acceptance Criteria

1. Session import runs only when `sessionBackend=postgres`.
2. Session import is skipped when `sessionBackend=filesystem`.
3. Warning log is emitted when filesystem and Postgres both already contain session domain data.
4. When same key exists in both stores (`state_id`, `session_id`, or `user_id`), Postgres record is preserved and filesystem conflicting record is skipped.
5. Filesystem session records that do not conflict are imported deterministically.
6. Import order is FK-safe and stable (`states -> handles -> user bindings`).
7. Repeated startup runs preserve stable session data state (no incorrect duplicates or drift).
8. No session HTTP/cookie contract drift is introduced.

## Test Cases And Scenarios

- Real startup integration checks:
  - filesystem-only session data imports correctly into empty Postgres,
  - overlapping filesystem+Postgres session data emits warning; conflicting keys keep Postgres values,
  - import order respects schema dependencies (`states -> handles -> user bindings`),
  - repeated startup runs preserve stable end state,
  - session import path is gated by `sessionBackend` value.
