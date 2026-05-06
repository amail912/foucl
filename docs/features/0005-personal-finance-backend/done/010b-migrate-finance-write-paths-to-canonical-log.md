# Migrate Finance Write Paths To Canonical Log

## Goal
As a backend maintainer, I want finance writes to append to the canonical event log, so all financial history is captured consistently in one event stream.

## Behavior And Business Rules
- Finance write flows append canonical events only to `finance_events`.
- Existing API success/failure semantics remain unchanged.
- Idempotency and validation behavior remain deterministic.
- Command handling must not persist partial states when append/update fails.

## Data And Contracts
- Write-path internals are refocused on canonical append flows established by `010a`.
- Public request/response contracts remain stable.
- Event-type payload and `event_version` semantics remain explicit for long-term evolution.
- Stream ordering and concurrency use `(stream_id, stream_version)` semantics.

## Technical Details
- Route finance command handlers through canonical event append logic.
- Maintain transactional consistency between canonical append and projection-table updates.
- Lock stream model decisions:
  - account aggregate stream id: `account:{accountId}`
  - transaction aggregate stream id: `transaction:{transactionId}`
  - `TransactionLinked` is owned by the source transaction stream and its payload includes both transaction ids.
- Use `(stream_id, stream_version)` for per-aggregate ordering and optimistic concurrency; keep `event_number` as global deterministic order.
- Keep canonical append helper minimal:
  - acquire transaction-scoped advisory lock per stream id
  - compute next stream version
  - append to `finance_events` with explicit `event_version = 1`.
- Keep domain error mapping stable (`AlreadyExists`, `NotFound`, `WriteFailure`, `StorageFailure`).
- Keep authorization and user-scoping guarantees unchanged.
- Legacy finance event tables remain present but are no longer write targets in runtime command paths.

## Testing
- Integration tests cover canonical append behavior across all finance write families.
- Existing finance behavior tests continue to pass without contract changes.
- Failure-path tests ensure no partial state persists on append/update errors.

## Rollout And Compatibility
- Backward-compatible at API level.
- Internal implementation shift only; no consumer migration required.
- Observability confirms canonical append health during rollout.
