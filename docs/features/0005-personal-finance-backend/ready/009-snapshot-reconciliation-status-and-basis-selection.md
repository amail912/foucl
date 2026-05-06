# Snapshot Reconciliation Status And Basis Selection

## Goal
As a user, I want to mark snapshots as reconciled, so discrepancy computation can use trusted checkpoint snapshots as balance bases.

## Behavior And Business Rules
- Story `006` still owns snapshot creation and the baseline reconciliation read shape.
- Snapshot reconciliation status is persisted separately from observed balance.
- New snapshots default to `unreconciled`.
- Only two statuses exist in v1: `unreconciled` and `reconciled`.
- A reconciled snapshot becomes a trusted basis for later reconciliation reads on the same account.
- A reconciled snapshot's own reconciliation view resolves to zero discrepancy at its own timestamp.
- Later reconciled snapshots supersede earlier reconciled snapshots for later target timestamps.
- When no reconciled snapshot exists at or before the target, reconciliation falls back to the current transaction-derived baseline behavior.

## Data And Contracts
- Defines `PUT /api/v1/finance/accounts/{id}/snapshots/{snapshotId}/reconciliation-status`.
- The request body contains `status`.
- `GET /api/v1/finance/accounts/{id}/snapshots` includes `reconciliationStatus` on each snapshot row.
- `GET /api/v1/finance/accounts/{id}/reconciliation` includes basis snapshot metadata:
  - `basisSnapshotId`
  - `basisSnapshotOccurredAt`
- `POST /api/v1/finance/accounts/{id}/snapshots` returns the reconciliation view for the created snapshot, including basis metadata and status.
- Unknown account or snapshot ids return `404`.
- Invalid status values return `400`.
- Status changes are idempotent.

## Technical Details
- Add canonical event type `BalanceSnapshotReconciliationStatusSet` to record status changes.
- Persist the current reconciliation status on the snapshot projection so list and reconciliation reads can select bases deterministically.
- Basis selection uses the latest reconciled snapshot on the same account with `occurredAt <= targetOccurredAt`, with deterministic tie-breaking for equal timestamps.
- Reconciliation reads derive the balance from the selected basis snapshot plus transaction deltas after that basis.
- Keep report aggregation behavior unchanged in this story.
- Track synthetic adjustment transactions as a separate follow-up story rather than in this scope.

## Testing
- Snapshot create defaults to unreconciled status.
- Marking a snapshot reconciled updates the snapshot read and reconciliation read views.
- Unmarking a snapshot returns it to unreconciled status.
- Reconciliation for a reconciled snapshot resolves to zero discrepancy at that snapshot.
- Reconciliation for a later snapshot uses the latest reconciled basis and remains deterministic across equal-timestamp snapshots.
- Reconciliation falls back to the current zero-baseline behavior when no reconciled basis exists.
- Existing snapshot create/list/reconciliation and export flows remain backward compatible apart from the intentional status additions.

## Rollout And Compatibility
- Backward-compatible because status is additive to existing snapshot data.
- No migration or backfill is required because no snapshot data exists yet.
- Adjustment-workflow follow-up remains separate and is not part of this story.
