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

### Ready
- [002b List Transactions](ready/002b-list-transactions.md) - Define the projection-backed ledger read contract, shared transaction-row shape, and half-open time filtering.
- [003a Manage Categories](ready/003a-manage-categories.md) - Define the seeded built-in category tree, selection rules, and user-category management contract.
- [003b Categorize And Split Transactions](ready/003b-categorize-and-split-transactions.md) - Define replaceable classification state, split semantics, and report-facing Uncategorized behavior.
- [004 Link Transfer Transactions](ready/004-link-transfer-transactions.md) - Define one-to-one transfer linking, invalid-pair rejection, and report exclusion for internal money movement.
- [005a Append Transaction Notes](ready/005a-append-transaction-notes.md) - Define note creation, blank-text rejection, and current note exposure in transaction rows.
- [005b Edit And Delete Transaction Notes](ready/005b-edit-and-delete-transaction-notes.md) - Define mutable note lifecycle, text validation, and soft-delete behavior on stable note ids.
- [006 Record And Apply Balance Snapshots](ready/006-record-and-apply-balance-snapshots.md) - Define snapshot-centered reconciliation writes, latest reconciliation reads, and additive balance-basis behavior.
- [007 Build Reporting And Ledger Projections](ready/007-build-reporting-and-ledger-projections.md) - Define split-aware aggregate report behavior, repeated-param filters, and deterministic projection reads.
- [008 Export Finance Data](ready/008-export-finance-data.md) - Define the versioned export wire contract with canonical events and convenience `views`.

### Done
- [001a Open And List Accounts](done/001a-open-and-list-accounts.md) - Define account creation, normalized per-user naming rules, and authenticated account listing behavior.
- [001b Close Accounts And Block New Transactions](done/001b-close-accounts-and-block-new-transactions.md) - Define account closure semantics and closed-account blocking for later transaction writes.
- [002a Record Income And Expense Transactions](done/002a-record-income-and-expense-transactions.md) - Define immutable manual money-entry creation, strict idempotency, and create-response semantics.

### Canceled

### Postponed
