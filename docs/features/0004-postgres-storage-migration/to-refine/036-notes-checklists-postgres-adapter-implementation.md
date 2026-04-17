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
