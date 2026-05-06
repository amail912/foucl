# Remove Legacy Finance Event Storage And Finalize Rollout

## Goal
As a backend maintainer, I want to retire legacy finance event storage paths, so finance event sourcing has one maintained canonical implementation.

## Behavior And Business Rules
- Canonical finance event log is the only maintained event-storage path.
- Legacy finance event storage paths are removed after cutover verification.
- Existing finance API behavior remains unchanged for consumers.
- Startup and migration flows remain deterministic and consistent with the new event model.

## Data And Contracts
- Removes obsolete internal storage artifacts no longer needed after canonical cutover.
- Keeps public API contracts stable.
- Updates finance migration/test expectations to canonical-only event storage.

## Technical Details
- Remove legacy event write/read plumbing and related startup assumptions.
- Keep projection contracts and domain error semantics intact.
- Align migration registration and integration reset/verification flows.
- Update supporting docs and backlog lifecycle status to reflect finalized architecture.

## Testing
- Full required test sequence passes after legacy removal:
  - `make test`
  - `make integration-test`
  - `make storage-migration-tests`
- Migration integration tests validate canonical-only finance storage expectations.
- Regression checks verify no behavior drift in existing finance endpoints.

## Rollout And Compatibility
- API-compatible cleanup step.
- Internal storage simplification after cutover confidence is established.
- No fallback to legacy event storage after this story is complete.
