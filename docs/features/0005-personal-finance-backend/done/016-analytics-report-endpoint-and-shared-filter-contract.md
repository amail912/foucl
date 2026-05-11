# Analytics Report Endpoint And Shared Filter Contract

## Goal
As a user, I want richer analytics payloads and consistent filter behavior between reports and ledger reads, so reports can drive charts, balances, and exact ledger navigation.

## Behavior And Business Rules
- Add endpoint:
  - `GET /api/v1/finance/report/analytics`
- `from` and `to` are required query parameters for analytics requests.
- Analytics endpoint filter surface must align with extended ledger filtering semantics, including:
  - `direction`
  - `accountId`
  - repeated `categoryIn`
  - repeated `categoryNotIn`
  - `amountMin`
  - `amountMax`
  - `search`
- Analytics response includes:
  - `summary`
  - `categoryBreakdown`
  - `cashflowSeries`
  - `accountBalances`
- Keep endpoint:
  - `GET /api/v1/finance/report`
- Report and analytics endpoints must share the same filter interpretation as `GET /api/v1/finance/transactions` for overlapping query dimensions.
- `cashflowSeries` uses deterministic automatic bucket granularity by effective range span:
  - span `<= 62 days`: daily buckets
  - span `<= 366 days`: weekly buckets
  - span `> 366 days`: monthly buckets
- `cashflowSeries` buckets are contiguous, non-overlapping, and cover the full `[from, to)` window.

## Data And Contracts
- `GET /api/v1/finance/report` fields remain:
  - `total`, `count`, `transactionIds`
- `GET /api/v1/finance/report/analytics` returns chart-ready sections with deterministic values for identical filter inputs.
- `GET /api/v1/finance/report/analytics` does not include `transactionIds`.
- API contract is defined as greenfield for this feature iteration.

## Technical Details
- Keep report computations projection-backed and deterministic.
- Reuse shared filter normalization so report and ledger paths cannot diverge for equivalent requests.
- Ensure totals and counts in analytics are internally consistent with report semantics.
- Keep category semantics aligned with split-aware report filtering behavior.

## Testing
- Analytics endpoint returns all required top-level sections.
- Equivalent filter inputs produce stable deterministic outputs.
- Report and analytics totals align for equivalent filters where metrics overlap.
- Missing `from` or `to` returns `400`.
- Invalid filter values return `400` consistently with ledger/report validation behavior.
- Auto-bucketing selects daily/weekly/monthly granularity at the configured range thresholds.
- Returned buckets are contiguous, non-overlapping, and cover the full requested window.
- Analytics responses do not include `transactionIds`.
- Equivalent report and ledger queries use consistent filter interpretation.

## Rollout And Compatibility
- New v1 contract surface for personal-finance reporting.
