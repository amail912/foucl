# 030 Calendar And Trip Sharing Postgres Parity Verification

## Objective

Verify calendar and trip-sharing behavior remains contract-compatible when the domains run on Postgres backends.

## Scope

- Real Postgres integration verification for:
  - calendar create/list/update/delete/validate flows,
  - trip-sharing shares/subscriptions add/list/delete flows,
  - period-trips visibility and ordering behavior.
- Validate status/message/payload compatibility at API boundaries.

## Verification Mode

- Real Postgres runtime verification is required; mock-only checks are insufficient.
- Unavailable Postgres test environment is a hard failure for this story.

## Dependencies And Boundaries

Depends on:

- 026 Calendar And Trip Sharing Backend Selection And Wiring.
- 029 Calendar And Trip Sharing Postgres Adapter Implementation.

Out of scope:

- Schema or adapter implementation changes.
- Startup import behavior.

## Acceptance Criteria

1. Real Postgres parity checks pass for all in-scope scenarios.
2. Calendar/trip-sharing/period-trips behavior remains contract-compatible.
3. No new client-visible error category is introduced.
