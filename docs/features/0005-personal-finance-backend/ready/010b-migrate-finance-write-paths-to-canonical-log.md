# Migrate Finance Write Paths To Canonical Log

## Goal
As a backend maintainer, I want finance writes to append to the canonical event log, so all financial history is captured consistently in one event stream.

## Behavior And Business Rules
- Finance writes append canonical events for account, transaction, classification, transfer-link, note, and snapshot changes.
- Write success and failure semantics for existing APIs remain unchanged.
- Idempotency and validation behavior remain deterministic.
- Append failures must not create partial write states.

## Data And Contracts
- Write-path internals are refocused on canonical event append flows.
- Existing public request/response shapes remain stable.
- Event payload/version strategy must remain explicit for long-term evolution.

## Technical Details
- Route finance command handlers through canonical event append logic.
- Maintain transactional consistency between event append and projection updates.
- Keep domain error mapping stable (`AlreadyExists`, `NotFound`, `WriteFailure`, `StorageFailure`).
- Keep authorization and user-scoping guarantees unchanged.

## Testing
- Integration tests cover canonical append behavior for each finance write family.
- Existing finance behavior tests continue to pass without contract changes.
- Failure-path tests ensure no partial state persists on append/write errors.

## Rollout And Compatibility
- Backward-compatible at API level.
- Internal implementation shift only; no consumer migration required.
- Observability should confirm canonical append health during rollout.
