# 027 Calendar And Trip Sharing Filesystem Adapter Parity

## Objective

Align filesystem implementations with the new repository contracts so filesystem remains behavior-compatible while Postgres work is in progress.

## Scope

- Implement contract-conformant filesystem adapters for calendar and trip-sharing.
- Preserve existing semantics:
  - calendar CRUD/validate behavior,
  - trip-sharing idempotent add/delete behavior,
  - deterministic list ordering.
- Emit only shared repository error categories.

## Dependencies And Boundaries

Depends on:

- 025 Calendar And Trip Sharing Repository Contracts.

Out of scope:

- Postgres schema or Postgres adapters.
- Runtime backend selection.
- Startup import behavior.

## Acceptance Criteria

1. Filesystem adapters satisfy the repository contracts for both domains.
2. Existing user-visible behavior remains contract-compatible.
3. Deterministic ordering and idempotent relation semantics are preserved.
4. Adapter error outputs are restricted to shared repository errors.
