# Record And Apply Balance Snapshots

## Goal
As a user, I want to record observed account balances, so I can compare event-derived balances with real-world account state.

## Behavior And Business Rules
- A balance snapshot records an observed balance for one account at a specific `occurredAt`.
- Recording a snapshot must not rewrite or replace transaction history.
- Snapshots are allowed on active and closed accounts.
- Duplicate snapshots for the same account and exact timestamp are rejected.
- A snapshot becomes the account's balance basis at its timestamp.
- Later transactions adjust forward from that snapshot basis.
- Reconciliation is computed from a snapshot and transaction history rather than stored as its own persisted object.
- Writing a snapshot returns reconciliation for the snapshot that was just created, even when the write is backdated.

## Data And Contracts
- Defines `POST /accounts/{id}/snapshots`, `GET /accounts/{id}/snapshots`, and `GET /accounts/{id}/reconciliation`.
- Snapshot write payload contains `balance` and `occurredAt`.
- Snapshot list rows contain `id`, `occurredAt`, and `balance`.
- Reconciliation responses contain `snapshotId`, `snapshotOccurredAt`, `observedBalance`, `derivedBalanceAtSnapshot`, and `discrepancy`.
- `POST /accounts/{id}/snapshots` returns `404` for unknown or foreign-scope accounts and `409` for duplicate account-plus-timestamp snapshots.
- `POST /accounts/{id}/snapshots` success returns reconciliation for the created snapshot, not necessarily the latest snapshot on the account.
- `GET /accounts/{id}/reconciliation` returns the latest reconciliation by default and accepts optional `snapshotId` for a specific snapshot.

## Technical Details
- Snapshot handling must remain compatible with the append-only event log.
- `BalanceSnapshotRecorded` is the canonical persisted event for snapshot writes.
- Reconciliation reads should be computed projections rather than separately stored reconciliation objects.
- Balance calculation rules must be explicit so observed-versus-derived mismatches are reproducible.
- The balance basis must fall back to pure transaction-derived behavior when no prior snapshot exists.
- Latest reconciliation still means the latest snapshot by `occurredAt`; backdated writes do not change what the POST success response represents.

## Testing
- Recording a snapshot for an active account succeeds.
- Recording a snapshot for a closed account succeeds.
- Recording a snapshot for an unknown account returns `404`.
- Recording a duplicate account-plus-timestamp snapshot returns `409`.
- Recording a backdated snapshot returns reconciliation for the created snapshot rather than a later snapshot already on the account.
- Latest reconciliation returns the most recent snapshot-centered discrepancy view.
- Reconciliation by `snapshotId` returns the selected snapshot-centered discrepancy view.
- Later transactions adjust forward from the selected snapshot basis without rewriting earlier events.

## Rollout And Compatibility
- Backward-compatible because this is a new API surface.
- Snapshot and reconciliation schema or projection changes should be additive.
