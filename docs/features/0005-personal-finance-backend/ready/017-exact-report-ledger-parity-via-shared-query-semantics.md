# Exact Report-Ledger Parity Via Shared Query Semantics

## Goal
As a user, I want report-to-ledger drill-down to return the exact transaction universe used by reports, so report values are explainable and navigable without mismatch.

## Behavior And Business Rules
- Exact drill-down is defined through equivalent query parameters sent to report and ledger endpoints.
- For identical effective queries, report and ledger must resolve the same transaction universe.
- Canonical parity query surface for report and ledger comparisons is:
  - required `from`
  - required `to` for report endpoints
  - optional `direction=sent|received|all`
  - optional `accountId`
  - optional repeated `categoryIn`
  - optional repeated `categoryNotIn`
  - optional `amountMin`
  - optional `amountMax`
  - optional `search`
- Exactness requirement applies to:
  - transfer-linked transaction treatment
  - split proportional contribution behavior
  - uncategorized handling
  - include/exclude category filters
  - amount range and direction behavior
  - metadata search behavior

## Data And Contracts
- Use existing queryable report and ledger endpoints:
  - `GET /api/v1/finance/report`
  - `GET /api/v1/finance/report/analytics`
  - `GET /api/v1/finance/transactions`
- Report-to-ledger parity does not rely on `accountIn` or `accountNotIn`; parity comparisons use the canonical surface above.
- No alternate drill-down query mode is introduced.
- Existing validation failures for invalid queries remain explicit `400` outcomes.

## Technical Details
- Define one shared query-normalization path for report and ledger filters.
- Ensure projection logic for report aggregation and ledger matching applies aligned inclusion semantics.
- Ensure parity comparisons include adjustment-row behavior as defined by report and ledger contracts.
- Preserve deterministic ordering and stable matching behavior for repeated identical queries.

## Testing
- For equivalent queries, report `transactionIds` match the ledger transaction id universe.
- Parity holds for filters involving splits, transfers, categories, direction, and amount ranges.
- Parity holds when metadata search is present.
- Parity holds for adjustment-row inclusion/exclusion behavior.
- Invalid queries are rejected consistently across report and ledger surfaces.

## Rollout And Compatibility
- New v1 contract requirement for report-to-ledger parity in personal-finance.
