# List Transactions

## Goal
As a user, I want to view my transaction history as a ledger, so I can inspect financial activity without reading raw events.

## Behavior And Business Rules
- `GET /api/v1/finance/transactions` returns transactions owned by the authenticated user only.
- Transactions are ordered by `occurredAt` descending by default.
- When multiple transactions share the same `occurredAt`, transaction `id` provides deterministic tie-break ordering.
- `accountId` filters the ledger to one account.
- `from` filters the ledger by `occurredAt` inclusively.
- `to` filters the ledger by `occurredAt` exclusively.
- Unknown or foreign-scope `accountId` filters return an empty list.
- `from == to` returns an empty list.
- `from > to` returns `400`.
- No pagination is exposed in v1.
- Transaction rows include the current transfer, category, split, and note summaries for each transaction.

## Data And Contracts
- Defines `GET /api/v1/finance/transactions`.
- Supports optional query parameters `accountId`, `from`, and `to`.
- Response rows use the v1 transaction-row shape: `id`, `direction`, `accountId`, `amount`, `occurredAt`, `recordedAt`, `transfer`, `category`, `splits`, and `notes`.
- In this implementation slice, `transfer` is `null`, `category` is `null`, `splits` is `[]`, and `notes` is `[]` until the later stories land.

## Technical Details
- Read behavior should come from SQL projections rather than rebuilding transaction history on each request.
- Filter behavior must apply to `occurredAt` consistently across repeated reads.
- Half-open time-range behavior must stay consistent between direct reads and any later drill-down from aggregate report results.
- List ordering must remain deterministic even when transactions share the same occurrence timestamp.
- The transaction-row shape should remain aligned with create-success responses from the money-entry endpoints.

## Testing
- Default transaction listing returns deterministic `occurredAt` descending order.
- Transactions with identical `occurredAt` values are ordered deterministically by `id`.
- `accountId` filtering returns only transactions for the selected account.
- `from` includes transactions exactly at the boundary timestamp.
- `to` excludes transactions exactly at the boundary timestamp.
- `from` and `to` filters behave deterministically for the same stored ledger.
- Transaction rows include transfer, category, split, and note summaries in the documented shape.

## Rollout And Compatibility
- Backward-compatible because this is a new API surface.
- Projection changes should be additive for new ledger read tables or indexes.
