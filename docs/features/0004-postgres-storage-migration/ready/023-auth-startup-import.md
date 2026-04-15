# 023 Auth Startup Import

## Objective

Add startup filesystem-to-Postgres bootstrap import for the auth domain when auth is switched to Postgres.

## Scope

- Run auth import only when `authBackend=postgres`.
- Require top-level `database` split config (`host`, `port`, `name`, `user`, `password`) to be present in postgres auth mode.
- Read filesystem auth records at startup.
- Read existing Postgres auth records at startup.
- If both stores contain auth data, log a warning with auth domain context.
- Import auth records into Postgres deterministically with explicit conflict handling.
- Keep import idempotent and safe on repeated startup runs.

## Import Conflict Policy

- Domain conflict key: `username`.
- Filesystem-only record -> insert/upsert into Postgres.
- Postgres-only record -> keep Postgres record unchanged.
- Record exists in both stores for same `username` -> Postgres record is authoritative; skip filesystem conflicting record and emit warning with domain/key context.

## Import Execution Contract

- Import executes during startup only.
- Import order for auth data is deterministic and stable across runs.
- Conflict decisions are deterministic (`postgres-wins` for duplicate key collisions).
- Import path must not modify HTTP-visible auth behavior.

## Dependencies And Boundaries

- Depends on:
  - 012 Auth Backend Selection And Wiring
  - 014 Auth Postgres Schema And Migrations
  - 015 Auth Postgres Adapter Implementation

Out of scope:

- Session startup import behavior (024).
- Calendar/trip-sharing/notes/checklists import.
- Non-startup migration tooling.

## Acceptance Criteria

1. Auth import runs only when `authBackend=postgres`.
2. Auth import is skipped when `authBackend=filesystem`.
3. Postgres auth mode fails startup before import if DB connect or auth schema sanity checks fail.
4. Warning log is emitted when filesystem and Postgres both already contain auth domain data.
5. When same `username` exists in both stores, Postgres record is preserved and filesystem conflicting record is skipped.
6. Filesystem auth records that do not conflict are imported deterministically.
7. Repeated startup runs preserve stable auth data state (no incorrect duplicates or drift).
8. No auth HTTP contract drift is introduced.

## Test Cases And Scenarios

- Real startup integration checks:
  - filesystem-only auth data imports correctly into empty Postgres,
  - overlapping filesystem+Postgres auth data emits warning; conflicting usernames keep Postgres values,
  - repeated startup runs preserve stable end state,
  - auth import path is gated by `authBackend` value.
