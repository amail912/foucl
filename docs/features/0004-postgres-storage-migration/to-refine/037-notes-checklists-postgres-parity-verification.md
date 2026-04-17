# 037 Notes And Checklists Postgres Parity Verification

## Objective

Verify notes and checklists behavior remains contract-compatible when domains run on Postgres backends.

## Scope

- Real Postgres integration verification for notes/checklists CRUD and version-conflict flows.
- Validate status/message/payload compatibility at API boundaries.
- Confirm no new client-visible error category is introduced.

## Verification Mode

- Real Postgres runtime verification is required; mock-only checks are insufficient.
- Unavailable Postgres test environment is a hard failure for this story.

## Dependencies And Boundaries

Depends on:

- 036 Notes And Checklists Postgres Adapter Implementation.

Out of scope:

- Schema or adapter implementation changes.
- Startup import behavior.

## Acceptance Criteria

1. Real Postgres parity checks pass for all in-scope scenarios.
2. Notes/checklists behavior remains contract-compatible.
3. No new client-visible error category is introduced.
