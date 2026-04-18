# Postgres Migration System

This document defines the SQL-first migration system used for Postgres-backed domains.

## Goals

- Keep schema evolution deterministic and reviewable through SQL files.
- Run migrations from Haskell code for repeatable test/dev flows.
- Support safe rollback for each migration through explicit down SQL.

## Structure

- Migration files are stored under `db/migrations/<domain>/`.
- Each migration has exactly two files:
  - `<NNNN>_<name>.up.sql`
  - `<NNNN>_<name>.down.sql`
- Ordering is lexical by migration id prefix (`NNNN`).

Example for auth:

- `db/migrations/auth/0001_auth_schema.up.sql`
- `db/migrations/auth/0001_auth_schema.down.sql`

Session migrations follow the same structure under `db/migrations/session/` and are expected to satisfy the repository contract operations defined by story 017 (handle/state/binding load/create/update/delete semantics).

## Execution Model

- Runner module: `PostgresMigrations`.
- Metadata table: `schema_migrations (migration_id TEXT PRIMARY KEY, applied_at TIMESTAMPTZ)`.
- `MigrateUp`:
  - Ensure metadata table exists.
  - Apply each unapplied migration in order.
  - Insert migration id into metadata table.
- `MigrateDown`:
  - Ensure metadata table exists.
  - Process applied migrations in reverse order.
  - Execute down SQL and remove id from metadata table.
- Each run is wrapped in a single DB transaction.

## Runtime Startup Behavior

- Startup runs automatic `MigrateUp` for every domain configured with backend `postgres` (`auth`, `session`, `calendar`, `trip-sharing`, `note`, `checklist`).
- Domains configured with backend `filesystem` are skipped by startup migration orchestration.
- Runtime startup sequence for Postgres-enabled domains is:
  1. startup migration orchestration (`MigrateUp`),
  2. Postgres storage wiring/verification,
  3. filesystem-to-Postgres startup import,
  4. HTTP serving.
- Startup migration is fail-fast: if any selected domain migration fails, startup exits and HTTP is not served.
- Startup migration idempotency relies on `schema_migrations`; already-applied migration ids are skipped.

## Operational Expectations

- Local/dev/test/prod runtime behavior is consistent: the server owns startup auto-migration for Postgres-enabled domains.
- External migration pre-application is optional for operational workflows, but it is not required for startup correctness.
- Teams should still review SQL migration files as deploy-time artifacts; startup auto-migrate does not change SQL review requirements.

## Auth Schema (0001)

Auth migration `0001_auth_schema` defines:

- Enum type `auth_user_role` with values `admin`, `member`.
- Table `auth_users` with:
  - `username` (`TEXT PRIMARY KEY`),
  - `password_hash` (`TEXT NOT NULL`),
  - `role` (`auth_user_role NOT NULL`),
  - `approved` (`BOOLEAN NOT NULL`).

`username` uniqueness is case-sensitive via Postgres text semantics (e.g. `Alice` and `alice` are distinct keys).

## Testing and Validation

Migration and startup-migration behavior is validated by two complementary test paths:

- Unit-level migration tests are env-gated (`FOUCL_TEST_POSTGRES_URL`) and verify direct migration runner behavior.
- Postgres integration tests (`make integration-test-postgres` / `cabal test foucl-integration-postgres-tests`) verify startup migration orchestration on real startup flows.

Validated scenarios:

1. Up migration on clean DB.
2. Down migration from latest state.
3. Re-apply up after down.
4. Schema assertions for table/columns/enum/boolean/uniqueness semantics.
5. Startup migration clean-DB bootstrap.
6. Startup migration no-op stability on pre-migrated DB.
7. Startup migration failure aborts startup before HTTP serving.

## Conventions for New Migrations

- Always ship up/down together.
- Keep domain-local migrations in the same domain folder.
- Write down SQL in reverse dependency order (drop dependent objects first).
- Avoid mixing unrelated domains in the same migration.
- For destructive changes, prefer additive migration + follow-up cleanup migration.
