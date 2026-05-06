# Remove Legacy Finance Event Storage And Finalize Rollout

## Goal
As a backend maintainer, I want to finalize canonical finance event sourcing and remove obsolete internals, so the backend has one maintained event-storage architecture.

## Behavior And Business Rules
- Canonical finance event log remains the only maintained event-storage path.
- Existing finance API behavior remains unchanged for consumers.
- Startup and migration flows remain deterministic and aligned with canonical-event architecture.

## Data And Contracts
- Removes obsolete internal code/test artifacts tied to old finance event storage paths.
- Keeps public API contracts stable.
- Updates migration/test expectations to canonical-only finance event storage.

## Technical Details
- Remove stale legacy plumbing no longer needed after `010a-010c` cutovers.
- Remove legacy finance event-table dependencies now that `010b` routes runtime writes only through `finance_events`.
- Keep projection contracts and domain error semantics intact.
- Align integration reset/verification flows and supporting docs to final canonical model.
- Preserve canonical stream decisions introduced in `010b` (`account:{accountId}`, `transaction:{transactionId}`, source-owned transfer-link events).
- Update backlog lifecycle and architecture notes to reflect finalized rollout.

## Testing
- Full required sequence passes after cleanup:
  - `make test`
  - `make integration-test`
  - `make storage-migration-tests`
- Migration integration tests validate canonical-only finance storage expectations.
- Regression checks verify no behavior drift in existing finance endpoints.

## Rollout And Compatibility
- API-compatible cleanup step.
- Internal simplification after canonical cutover confidence is established.
- No fallback to deprecated legacy storage paths after this story.
