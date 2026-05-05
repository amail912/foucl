# Snapshot Reconciliation Status And Basis Selection

## Goal
As a user, I want to mark snapshots as reconciled so discrepancy computation can use trusted checkpoint snapshots as balance bases.

## Problem Context
- Story `006` computes reconciliation from snapshots and transaction history without persisted reconciliation status.
- This makes all snapshots equivalent in basis selection.
- We need a follow-up design to distinguish trusted (reconciled) snapshots from non-reconciled ones.

## Questions To Refine
- What lifecycle states are needed (for example: unreconciled, reconciled)?
- Which endpoint marks or unmarks reconciliation status?
- How should latest reconciled snapshot influence derived-balance computation for later snapshots?
- What should happen when no reconciled snapshot exists?
- What migration/backfill behavior is required for existing snapshots?

## Expected Outcomes
- Explicit API and data model for snapshot reconciliation status.
- Deterministic discrepancy-computation rules using reconciled basis selection.
- Backward-compatible migration and test coverage plan.
