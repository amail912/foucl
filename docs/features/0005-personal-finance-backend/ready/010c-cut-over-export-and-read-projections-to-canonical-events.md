# Cut Over Export And Read Projections To Canonical Events

## Goal
As a backend maintainer, I want export and projection rebuild flows to consume canonical events, so all derived finance views are deterministically reproducible.

## Behavior And Business Rules
- Export canonical section is sourced from `finance_events`.
- Convenience `views` remain projection-backed and keep existing behavior contracts.
- Rebuild-from-events and incremental projection updates must remain equivalent.
- Canonical replay ordering uses `event_number`.

## Data And Contracts
- Refines `GET /api/v1/finance/export` canonical `events` sourcing.
- Keeps `views` contract shape and exclusion rules stable (`reconciliation` and `report` excluded from views).
- Keeps transaction-row view shape parity with `GET /api/v1/finance/transactions`.
- No legacy canonical-event source is maintained after cutover.

## Technical Details
- Switch export canonical-event reads to `finance_events` as source of truth.
- Align projection replay/rebuild paths to canonical event consumption in `event_number` order.
- Preserve existing query semantics for ledger, report, categories, notes, and snapshots.
- Keep user scoping and deterministic ordering explicit in read paths.

## Testing
- Export tests verify canonical event sourcing and stable top-level contract.
- Projection parity tests verify incremental vs rebuild equivalence.
- Existing finance read behavior tests remain green.

## Rollout And Compatibility
- Backward-compatible API behavior.
- Internal source-of-truth cutover from old event storage to canonical log.
- Rollout includes parity verification before cleanup story `010d`.
