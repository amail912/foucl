# Extended Ledger Filtering Contract

## Goal
As a user, I want richer ledger filtering controls, so I can isolate precise subsets of transactions from the backend without client-side workarounds.

## Behavior And Business Rules
- Extend `GET /api/v1/finance/transactions` while preserving existing parameters.
- Existing supported parameters remain:
  - `accountId`
  - `from` (inclusive)
  - `to` (exclusive)
- Add parameters:
  - `direction=sent|received|all` (default `all`)
  - repeated `categoryIn`
  - repeated `categoryNotIn`
  - `amountMin`
  - `amountMax`
  - `search`
- `direction` semantics:
  - `sent` returns only sent transactions.
  - `received` returns only received transactions.
  - `all` returns sent, received, and adjustment rows under existing ledger behavior.
- `search` is case-insensitive and matches `counterparty` and `description`.
- Counterparty matching benefits from canonical lowercase counterparty persistence, while description matching remains case-insensitive.
- Category filter semantics mirror report-category behavior:
  - when a split is active, category filters evaluate active split categories
  - when no split is active, category filters evaluate the whole-transaction category
- Amount-range semantics:
  - `amountMin` and `amountMax` compare against absolute row `amount` (integer cents), not signed report totals.
- Validation:
  - `amountMin` and `amountMax` must be integer cents when provided.
  - `amountMin <= amountMax` when both are present.
  - `categoryIn` and `categoryNotIn` must not overlap.
  - `from > to` returns `400`, while `from == to` returns an empty list.
  - Invalid values return `400`.
- Keep current sort order semantics unchanged.

## Data And Contracts
- Existing transaction row shape remains, including additive metadata fields from Story `012`.
- This story defines the v1 extended filter contract for ledger reads.

## Technical Details
- Reuse existing report/ledger filter normalization conventions where possible.
- Keep deterministic filtering semantics for split-classified and transfer-linked rows consistent with current ledger behavior.
- Keep category filtering semantics aligned with report filtering so equivalent report/ledger queries can satisfy Story `017` parity requirements.
- Ensure query/index strategy supports added filter dimensions without regressions.

## Testing
- `direction` filters sent/received/all correctly.
- Repeated category include/exclude filters behave deterministically.
- Overlap between include/exclude category sets returns `400`.
- Amount range filters include boundaries correctly using absolute row `amount` and reject invalid ranges.
- Non-integer `amountMin` or `amountMax` returns `400`.
- Split-active category filtering uses split categories; non-split filtering uses whole-transaction category.
- `search` matches metadata fields case-insensitively.
- Unknown or foreign `accountId` filters return an empty list.
- `from > to` returns `400`, and `from == to` returns an empty list.
- Existing ledger query usage without new params remains unchanged.

## Rollout And Compatibility
- v1 extended ledger-filter contract definition for `/api/v1/finance`.
