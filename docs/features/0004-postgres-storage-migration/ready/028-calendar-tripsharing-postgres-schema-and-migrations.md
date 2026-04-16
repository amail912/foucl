# 028 Calendar And Trip Sharing Postgres Schema And Migrations

## Objective

Define SQL schema and migrations for calendar and trip-sharing persistence using the migration system conventions already used by auth/session.

## Scope

- Add domain migrations under:
  - `db/migrations/calendar/`
  - `db/migrations/trip-sharing/`
- Define keys, constraints, and indexes required by contract operations and period-trips reads.
- Keep migration execution compatible with the shared `PostgresMigrations` runner.

## Dependencies And Boundaries

Depends on:

- 025 Calendar And Trip Sharing Repository Contracts.

Out of scope:

- Repository adapter implementation details.
- Runtime backend selection.
- Startup import behavior.

## Acceptance Criteria

1. Calendar/trip-sharing schemas support all required repository operations.
2. Migrations provide explicit up/down SQL and follow ordering conventions.
3. Schema checks can fail fast at startup in postgres backend mode.
4. Migration behavior is deterministic and reversible in test/dev flows.
