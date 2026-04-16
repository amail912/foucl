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

## Implementation Decisions

- Added new migration domains:
  - `db/migrations/calendar/0001_calendar_schema.up.sql|down.sql`
  - `db/migrations/trip-sharing/0001_trip_sharing_schema.up.sql|down.sql`
- Calendar schema uses a fully normalized single-table model (`calendar_items`) with:
  - composite key `(user_id, item_id)`,
  - explicit `item_kind` discriminator (`legacy|trip`),
  - variant-specific columns for legacy and trip payloads,
  - CHECK constraints enforcing valid variant shape and recurrence constraints.
- Trip-sharing schema uses two relation tables with composite primary keys to enforce idempotent add semantics:
  - `trip_shares(owner_user_id, target_username)`,
  - `trip_subscriptions(owner_user_id, target_username)`.
- Added migration runner support in `PostgresMigrations` for both new domains:
  - `runCalendarMigrations(AtPath)`,
  - `runTripSharingMigrations(AtPath)`.
- Added migration unit coverage for both domains:
  - up/down/up repeatability,
  - schema/table/index assertions,
  - representative uniqueness/constraint behavior checks.
- Startup fail-fast schema verification wiring remains deferred to story 029; 028 delivers schema artifacts and migration execution support only.
