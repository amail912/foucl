# 039 Startup Migration Orchestrator

## Objective

Run Postgres `MigrateUp` automatically at server startup for all domains configured with `postgres`, before schema verification and before serving HTTP traffic.

## Scope

- Add startup orchestration for migration execution in app boot flow.
- Resolve active migration domains from selected backend flags:
  - auth
  - session
  - calendar
  - trip-sharing
  - note
  - checklist
- Run startup migrations before existing `verifyPostgres*Storage` checks.
- Keep startup fail-fast behavior: migration errors abort boot and prevent HTTP server start.
- Keep startup migration idempotent by relying on `schema_migrations`.

## Dependencies And Boundaries

Depends on:

- Existing `PostgresMigrations` runner and migration definitions.
- Existing backend-selection wiring in `Lib`.

Out of scope:

- New migration SQL files.
- Non-Postgres backend behavior changes.

## Acceptance Criteria

1. Startup executes `MigrateUp` for each Postgres-enabled domain and skips filesystem-only domains.
2. Migration execution occurs before storage verification and before startup import.
3. If a migration step fails, startup exits non-zero and does not start HTTP serving.
4. If no migrations are pending, startup continues successfully without schema drift.
