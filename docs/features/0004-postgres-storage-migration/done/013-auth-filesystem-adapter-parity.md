# 013 Auth Filesystem Adapter Parity

## Objective

Implement a filesystem-backed auth repository adapter that satisfies the new auth repository contract with parity behavior.

## Scope

- Implement filesystem adapter for the auth repository contract.
- Emit only shared generic repository errors from the common `Repository` module, not backend-specific error types.
- Preserve current auth persistence semantics, including pending/approved handling and admin governance behavior.
- Ensure adapter output and error semantics align with existing auth business expectations.
- Keep filesystem mode behavior stable after contract extraction.

## Acceptance Criteria

1. In filesystem mode, auth behavior remains contract-compatible.
2. Existing auth-focused integration tests pass without behavior regressions in filesystem mode.
3. Filesystem adapter satisfies the full auth repository contract.
4. Duplicate/missing/read/write/storage failures map to shared repository errors (`AlreadyExists`, `NotFound`, `ReadFailure`, `WriteFailure`, `StorageFailure`).
5. No HTTP contract drift is introduced.

## Out Of Scope

- Postgres auth repository implementation.
- Startup import behavior.
