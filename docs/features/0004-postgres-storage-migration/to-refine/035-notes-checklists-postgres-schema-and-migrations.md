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
