# Personal Finance Backend

This backlog captures the backend work needed to support single-user personal finance tracking with immutable event-sourced financial facts and SQL projections for reads.

## Goal
Provide an authenticated personal finance backend for manual transaction entry, account tracking, reporting, and export so a user can maintain an auditable financial history without external integrations or automation.

## Scope For This Iteration
- Single-user finance data scoped to the authenticated user.
- Event-sourced writes for accounts, transactions, links, notes, and balance snapshots.
- SQL projections for ledger, aggregate report, reconciliation, and related read flows.
- Explicit transfer linking instead of automatic transfer detection.
- No multi-user finance sharing, no integrations, and no multi-currency support.

## Supporting Documents
- [API Contract](api-contract.md) - Draft endpoint surface, request intent, and response semantics to refine with the stories.
- [Data Model](data-model.md) - Draft domain entities, event model, invariants, and projection expectations.

## Stories

### To Refine
- [009 Snapshot Reconciliation Status And Basis Selection](to-refine/009-snapshot-reconciliation-status-and-basis-selection.md) - Define persisted snapshot reconciliation status and trusted-basis selection rules for future discrepancy computation.

### Ready
- [008 Export Finance Data](ready/008-export-finance-data.md) - Define the versioned export wire contract with canonical events and convenience `views`.
- [010a Unify Finance Event Log Foundation](ready/010a-unify-finance-event-log-foundation.md) - Establish one canonical append-only finance event-log foundation and deterministic ordering guarantees.
- [010b Migrate Finance Write Paths To Canonical Log](ready/010b-migrate-finance-write-paths-to-canonical-log.md) - Route finance write behavior through canonical event append flows while keeping API semantics stable.
- [010c Cut Over Export And Read Projections To Canonical Events](ready/010c-cut-over-export-and-read-projections-to-canonical-events.md) - Switch export canonical sourcing and projection rebuild inputs to canonical events with parity guarantees.
- [010d Remove Legacy Finance Event Storage And Finalize Rollout](ready/010d-remove-legacy-finance-event-storage-and-finalize-rollout.md) - Retire legacy finance event storage paths after cutover verification and align migrations/tests/docs.

### Done
- [001a Open And List Accounts](done/001a-open-and-list-accounts.md) - Define account creation, normalized per-user naming rules, and authenticated account listing behavior.
- [001b Close Accounts And Block New Transactions](done/001b-close-accounts-and-block-new-transactions.md) - Define account closure semantics and closed-account blocking for later transaction writes.
- [002a Record Income And Expense Transactions](done/002a-record-income-and-expense-transactions.md) - Define immutable manual money-entry creation, strict idempotency, and create-response semantics.
- [002b List Transactions](done/002b-list-transactions.md) - Define the projection-backed ledger read contract, shared transaction-row shape, and half-open time filtering.
- [003a Manage Categories](done/003a-manage-categories.md) - Define the seeded built-in category tree, selection rules, and user-category management contract.
- [003b Categorize And Split Transactions](done/003b-categorize-and-split-transactions.md) - Define replaceable classification state, split semantics, and report-facing Uncategorized behavior.
- [004 Link Transfer Transactions](done/004-link-transfer-transactions.md) - Define one-to-one transfer linking, invalid-pair rejection, and report exclusion for internal money movement.
- [005a Append Transaction Notes](done/005a-append-transaction-notes.md) - Define note creation, blank-text rejection, and current note exposure in transaction rows.
- [005b Edit And Delete Transaction Notes](done/005b-edit-and-delete-transaction-notes.md) - Define mutable note lifecycle, trim-on-write note rules, and soft-delete behavior on stable note ids.
- [006 Record And Apply Balance Snapshots](done/006-record-and-apply-balance-snapshots.md) - Define snapshot-centered reconciliation writes, latest reconciliation reads, and additive balance-basis behavior.
- [007 Build Reporting And Ledger Projections](done/007-build-reporting-and-ledger-projections.md) - Define split-aware aggregate report behavior, repeated-param filters, and deterministic projection reads.

### Canceled

### Postponed
