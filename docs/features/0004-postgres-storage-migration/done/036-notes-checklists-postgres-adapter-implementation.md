# 036 Notes And Checklists Postgres Adapter Implementation

## Objective

Implement Postgres-backed adapters for notes and checklists repositories.

## Scope

- Implement CRUD operations for notes/checklists on Postgres.
- Preserve optimistic versioning semantics and conflict behavior.
- Map storage errors to shared error categories used by existing API behavior.
- Integrate adapters into backend wiring path from 033.

## Dependencies And Boundaries

Depends on:

- 033 Notes And Checklists Backend Selection And Wiring.
- 035 Notes And Checklists Postgres Schema And Migrations.

Out of scope:

- Real Postgres parity certification.
- Startup import behavior.

## Acceptance Criteria

1. Postgres adapters satisfy full notes/checklists contract operations.
2. Adapter errors map to existing shared error categories only.
3. Notes/checklists runtime uses Postgres adapters when switched to `postgres`.

## Implementation Decisions

- Implemented adapters with `postgresql-simple` only (no beam dependency).
- Added repository constructors and storage validators:
  - `postgresNoteRepository` / `verifyPostgresNoteStorage`
  - `postgresChecklistRepository` / `verifyPostgresChecklistStorage`
- Preserved optimistic version semantics on update:
  - stale version returns `NotCurrentVersion`,
  - missing item id maps to existing modification read-failure path.
- Preserved existing API-visible behavior:
  - delete-by-id remains idempotent for missing ids,
  - list skips malformed persisted payloads and logs parse failures.
- Replaced 033 temporary wiring stubs with real Postgres validation + repository composition in `makeNoteRepository` and `makeChecklistRepository`.
