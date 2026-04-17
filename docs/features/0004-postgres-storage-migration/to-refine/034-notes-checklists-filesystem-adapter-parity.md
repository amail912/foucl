# 034 Notes And Checklists Filesystem Adapter Parity

## Objective

Verify filesystem adapters for notes and checklists remain contract-compatible after repository/wiring extraction.

## Scope

- Validate CRUD behavior for filesystem-backed notes/checklists against contract from 032.
- Validate optimistic version conflict behavior remains unchanged.
- Keep status/message/payload semantics aligned with existing API contract.

## Dependencies And Boundaries

Depends on:

- 032 Notes And Checklists Repository Contracts.
- 033 Notes And Checklists Backend Selection And Wiring.

Out of scope:

- Postgres schema/migrations.
- Postgres adapter implementation.
- Startup import behavior.

## Acceptance Criteria

1. Filesystem parity checks pass for notes and checklists CRUD scenarios.
2. Optimistic version conflict behavior remains compatible.
3. No client-visible contract drift is introduced.
