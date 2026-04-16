# 019 Session Filesystem Adapter Parity

## Objective

Implement a filesystem-backed session repository adapter that satisfies the `SessionRepository` contract with parity behavior.

## Scope

- Implement filesystem adapter for all session repository operations defined by 017.
- Emit only shared generic repository errors from the common `Repository` module.
- Preserve current session persistence semantics for resolve/create, idle refresh, revoke single session, and revoke-all behavior through the stable `SessionStore` API.
- Keep filesystem mode behavior stable after contract extraction.

## Operational Contract

- Adapter implements full session repository operation set from 017.
- Adapter does not expose filesystem-specific errors or data-layout concerns to callers.
- Adapter preserves deterministic behavior for delete/revoke paths at business boundary.

## Error Mapping Contract

- Duplicate-write conflicts -> `AlreadyExists`
- Missing targeted record/state -> `NotFound`
- Read/decode failures -> `ReadFailure`
- Write/update/delete failures -> `WriteFailure`
- Storage-layer failures -> `StorageFailure`

## Verification Mode

- This story requires real filesystem integration verification.
- Mock-only verification is insufficient for parity signoff in this story.

## Acceptance Criteria

1. Filesystem adapter satisfies full `SessionRepository` operation contract.
2. Adapter emits only shared repository errors (`AlreadyExists`, `NotFound`, `ReadFailure`, `WriteFailure`, `StorageFailure`).
3. In filesystem mode, session behavior remains contract-compatible.
4. Session-focused integration tests pass without filesystem-mode regressions.
5. No HTTP route, payload, status code, cookie behavior, or message drift is introduced.

## Test Cases And Scenarios

- Integration scenarios in filesystem mode:
  - create and resolve session,
  - idle refresh/touch behavior,
  - revoke single session,
  - revoke-all-for-session/user.
- Error-path checks:
  - missing state/handle surfaces `NotFound`,
  - read/write/storage failures map to shared repository errors.

## Out Of Scope

- Postgres session repository implementation/parity.
- Startup import behavior.
