# 035 Notes And Checklists Postgres Schema And Migrations

## Objective

Provide Postgres schema and migrations for notes and checklists repositories.

## Scope

- Add SQL migrations for notes/checklists storage in domain migration folders.
- Define constraints/indexes required for deterministic CRUD and version semantics.
- Validate up/down/reapply migration behavior in tests.

## Dependencies And Boundaries

Depends on:

- 032 Notes And Checklists Repository Contracts.

Out of scope:

- Postgres adapter implementation.
- Backend cutover verification.
- Startup import behavior.

## Acceptance Criteria

1. Notes/checklists schema migrations apply successfully on clean DB.
2. Down migrations and reapply flow are validated.
3. Schema shape supports contract-compatible CRUD and version semantics.

## Implementation Decisions

- Added dedicated migration domains and first migration files:
  - `db/migrations/note/0001_note_schema.up.sql|down.sql`
  - `db/migrations/checklist/0001_checklist_schema.up.sql|down.sql`
- Notes/checklists schema shape uses one table per domain (`note_items`, `checklist_items`) with:
  - `item_id` text primary key,
  - `item_version` text for optimistic concurrency semantics,
  - `item_content` JSONB for persisted domain payload.
- Added explicit non-empty checks for `item_id` and `item_version` to prevent invalid persisted identifiers/versions.
- Added migration runner support in `PostgresMigrations`:
  - `runNoteMigrations(AtPath)`
  - `runChecklistMigrations(AtPath)`
- Added migration unit coverage for both new domains:
  - up/down/up repeatability,
  - schema/table/index assertions,
  - representative primary-key and version-check constraint behavior.
