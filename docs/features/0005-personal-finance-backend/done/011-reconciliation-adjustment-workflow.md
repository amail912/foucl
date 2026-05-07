# Reconciliation Adjustment Workflow

## Goal
As a user, I want unsupported balance differences to be captured as first-class reconciliation adjustments, so my finance history stays auditable without misclassifying corrections as ordinary spending or income.

## Behavior And Business Rules
- Story `009` owns persisted snapshot status and trusted basis selection.
- Reconciliation adjustments are snapshot-scoped and represent the signed correction needed to settle one snapshot.
- Adjustments are first-class financial events, but they are not ordinary money-entry transactions.
- Each snapshot has at most one current adjustment in projection state.
- A later adjustment for the same snapshot supersedes the earlier one in current reads while preserving event history.
- Creating an adjustment auto-marks the target snapshot as `reconciled`.
- The backend derives the adjustment amount from the selected snapshot's current discrepancy; clients do not supply the amount directly.
- A free-text `reason` is optional and exists for auditability.
- Adjustment rows remain distinguishable from normal transactions in ledger, report, and export convenience views.
- Adjustment rows contribute to P&L through the same signed sent/received aggregation path used by the existing report model.

## Data And Contracts
- Defines `POST /api/v1/finance/accounts/{id}/snapshots/{snapshotId}/adjustment`.
- The request body contains optional `reason`.
- Success returns the updated snapshot reconciliation state and the current adjustment summary for that snapshot.
- The adjustment summary is surfaced in convenience views with an explicit adjustment marker.
- `GET /api/v1/finance/transactions` includes adjustment rows in the shared row shape with an explicit marker.
- Export convenience views include adjustment rows with the same marker.
- Unknown account or snapshot ids return `404`.
- Invalid adjustment creation requests return `400` when the request body is malformed.

## Technical Details
- Add canonical event type `BalanceSnapshotAdjustmentRecorded` to record adjustment creation.
- Persist the current effective adjustment per snapshot in projection state so reads can select the latest adjustment deterministically.
- Projection selection uses the latest adjustment for the snapshot by deterministic recorded ordering, with later adjustments superseding earlier ones for the same snapshot.
- Adjustment rows reuse the shared ledger/report row shape and carry a marker that distinguishes them from ordinary transactions.
- Adjustment rows are signed so report aggregates can include them through the existing sent/received flow.
- Keep snapshot status and basis selection behavior from `009` unchanged.

## Testing
- Creating an adjustment derives the expected amount from the target snapshot discrepancy.
- Creating an adjustment auto-marks the target snapshot reconciled.
- A later adjustment for the same snapshot supersedes the previous one in current reads.
- Ledger and export views show adjustment rows with the explicit marker.
- Report totals include adjustment rows by sign.
- Unknown or foreign-scope account and snapshot ids return `404`.

## Rollout And Compatibility
- This story is additive and does not require migration or backfill.
- Adjustment history is append-only.
- There is no separate adjustment list endpoint in v1; the workflow is surfaced through snapshot, ledger, report, and export views.
