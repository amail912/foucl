# Unify Finance Event Log Foundation

## Goal
As a backend maintainer, I want one canonical finance event log, so event ordering, export, and replay can rely on one deterministic source of truth.

## Behavior And Business Rules
- Finance domain events are represented in one append-only canonical log.
- Event ordering must be deterministic and independent from projection query behavior.
- Canonical event history remains the source of truth for replay and audit workflows.
- Existing finance endpoint behavior remains unchanged while this foundation is introduced.

## Data And Contracts
- Introduces a canonical finance event store shape for all finance event types.
- Existing public API contracts remain backward-compatible in this story.
- Event identity, ordering fields, and version markers must be explicit in storage.
- Existing projection tables remain available for current read contracts.

## Technical Details
- Add foundational schema and wiring for the unified finance event log.
- Keep replay/export compatibility goals aligned with `data-model.md` and `api-contract.md`.
- Ensure append-only semantics and deterministic ordering are enforceable.
- Defer consumer cutover to later stories in this chain.

## Testing
- Migration test verifies canonical event log schema is created successfully.
- Ordering and identity constraints are validated at storage level.
- Startup migration tests confirm no regressions in non-finance domains.

## Rollout And Compatibility
- Backward-compatible for API consumers.
- Internal storage change is additive in this step.
- No external contract cutover in this story.
