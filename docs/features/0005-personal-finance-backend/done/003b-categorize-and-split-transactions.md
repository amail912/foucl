# Categorize And Split Transactions

## Goal
As a user, I want to classify transactions fully or partially, so aggregate report reads reflect how money was actually used.

## Behavior And Business Rules
- The backend must support categorizing a transaction as a whole.
- The backend must support splitting one transaction into multiple categorized parts.
- Splits do not create new transactions.
- Split amounts must sum exactly to the original transaction amount.
- Split payloads must contain at least two rows.
- Split rows may repeat the same category slug within one split payload.
- A whole-transaction category may be replaced by a later categorize request while no split is active.
- A split supersedes any existing whole-transaction category in current state.
- A later split replaces the current active split state.
- A whole-transaction categorize request is rejected while a split is active.
- Active split state overrides whole-transaction categorization in ledger summaries and reports.
- Explicit `Uncategorized Expense` and `Uncategorized Income` remain valid selectable categories.

## Data And Contracts
- Defines `POST /api/v1/finance/transactions/{id}/categorize` and `POST /api/v1/finance/transactions/{id}/split`.
- `POST /api/v1/finance/transactions/{id}/categorize` accepts `category` (slug).
- `POST /api/v1/finance/transactions/{id}/split` accepts `splits`, with each split row containing `category` (slug) and `amount`.
- `POST /api/v1/finance/transactions/{id}/categorize` returns `404` for unknown transactions or unknown category slugs, `400` for non-selectable category slugs, and `409` when a split is active.
- `POST /api/v1/finance/transactions/{id}/split` returns `404` for unknown transactions or unknown category slugs, `400` for non-selectable category slugs, fewer than two split rows, or invalid split totals.
- Success responses return the standard transaction row shape from the ledger contract.
- Report-side `uncategorized` matching includes transactions with no active category state and transactions explicitly assigned the built-in Uncategorized categories, including split rows.

## Technical Details
- Event append logic should keep transaction money facts immutable while allowing later classification events.
- Current-state derivation must prefer active split state over whole-transaction category state consistently across ledger and reporting projections.
- Re-categorization should replace current whole-category state through a later event while no split is active.
- Re-splitting should replace current split state through a later event rather than in-place mutation.
- Validation should remain deterministic and centralized so command behavior and projection behavior cannot diverge.

## Testing
- Categorizing an uncategorized transaction succeeds.
- Categorizing an already categorized transaction with a different selectable category succeeds and replaces current whole-category state.
- Categorizing or splitting with a non-selectable top-level built-in category returns `400`.
- Splitting an uncategorized transaction with at least two rows summing to the original amount succeeds.
- Splitting a categorized transaction succeeds and supersedes the whole-transaction category in current state.
- Submitting a second split replaces the first split in current state.
- Categorize requests against transactions with an active split return `409`.
- Splitting with totals that do not match the transaction amount returns `400`.
- Splitting with fewer than two rows returns `400`.
- Splitting with repeated category slugs in separate rows succeeds.
- Report `uncategorized` matching includes transactions with no active category state and transactions explicitly assigned the built-in Uncategorized categories.

## Rollout And Compatibility
- Backward-compatible because this is a new API surface.
- Any schema or projection changes for classification state should be additive.
