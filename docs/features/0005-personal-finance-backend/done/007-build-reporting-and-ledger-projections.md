# Build Reporting And Ledger Projections

## Goal
As a user, I want ledger and aggregate report views, so I can inspect history and analyze money movement without reading raw events directly.

## Behavior And Business Rules
- Ledger views include all transactions, including transfer-linked transactions.
- `GET /api/v1/finance/report` returns aggregate values for the filtered transaction set.
- Transfer-linked transactions are excluded from all report aggregates.
- Active split state overrides whole-transaction categorization in report matching and aggregation.
- When split-driven category filtering is active, aggregate totals sum only the matching split-row amounts.
- Transactions matched through one or more split rows still contribute once to `count` and once to `transactionIds`.
- `direction=all` returns net balance delta with income positive and spending negative.
- Category filters may target explicit category ids or the special `uncategorized` token.
- The `uncategorized` token matches transactions with no active category state and transactions explicitly assigned `Uncategorized Expense` or `Uncategorized Income`, including split rows.

## Data And Contracts
- Refines `GET /api/v1/finance/transactions` and defines `GET /api/v1/finance/report`.
- `GET /api/v1/finance/report` requires `from` and `to`.
- `GET /api/v1/finance/report` supports optional `direction`, `accountIn`, `accountNotIn`, `categoryIn`, and `categoryNotIn` using repeated query parameters in v1.
- `GET /api/v1/finance/report` returns `total`, `count`, and `transactionIds`.
- `transactionIds` expose the ids of the transactions included in the aggregate result.
- Overlapping include and exclude values for the same filter dimension return `400`.
- Split-category matching, Uncategorized behavior, and include/exclude filter combinations must remain deterministic.

## Technical Details
- SQL projections are the source of truth for ledger and report reads in this feature.
- Report queries should operate on projection state rather than event replay on each request.
- Reclassification caused by categorization, splitting, or transfer linking must update report projections deterministically.
- Category-filtered split transactions must aggregate matching split rows only while keeping `count` and `transactionIds` transaction-based.
- Note changes and snapshot writes must not change report aggregates.
- Incremental projection updates and full rebuild behavior must produce equivalent report results.

## Testing
- Ledger reads include expense, income, and transfer-linked transactions.
- `GET /api/v1/finance/report` with `direction=sent` excludes transfers and sums only expense transactions.
- `GET /api/v1/finance/report` with `direction=received` excludes transfers and sums only income transactions.
- `GET /api/v1/finance/report` with `direction=all` returns net balance delta.
- Category filters match active split rows when a split is active.
- Split transactions contribute only matching split-row amounts to `total`.
- Split transactions with multiple matching split rows contribute once to `count` and once to `transactionIds`.
- The `uncategorized` filter matches transactions with no active category state and transactions explicitly assigned the built-in Uncategorized categories, including split rows.
- Overlapping include and exclude values for the same filter dimension return `400`.
- Include and exclude account or category filters behave deterministically.
- Projection rebuild results match incremental report results for the same event history.

## Rollout And Compatibility
- Backward-compatible because this is a new API surface.
- Projection migrations and indexes should be introduced additively.
