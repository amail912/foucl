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

Migration integration tests are env-gated and run only when `FOUCL_TEST_POSTGRES_URL` is set.

Validated scenarios:

1. Up migration on clean DB.
2. Down migration from latest state.
3. Re-apply up after down.
4. Schema assertions for table/columns/enum/boolean/uniqueness semantics.

## Conventions for New Migrations

- Always ship up/down together.
- Keep domain-local migrations in the same domain folder.
- Write down SQL in reverse dependency order (drop dependent objects first).
- Avoid mixing unrelated domains in the same migration.
- For destructive changes, prefer additive migration + follow-up cleanup migration.
