# Close Accounts And Block New Transactions

## Goal
As a user, I want to close finance accounts without losing their history, so inactive accounts stop accepting new transactions while remaining part of my audit trail.

## Behavior And Business Rules
- Closing an account marks it as closed.
- Closing an already closed account is idempotent success.
- Closing an unknown account returns `404`.
- Closing an account does not delete its history or remove it from `GET /api/v1/finance/accounts?status=closed` or `GET /api/v1/finance/accounts?status=all`.
- Closed accounts reject new transaction creation through both `POST /api/v1/finance/transactions/sent` and `POST /api/v1/finance/transactions/received`.
- Reopening accounts is out of scope for v1.

## Data And Contracts
- Defines `POST /api/v1/finance/accounts/{id}/close`.
- Success response returns the closed account projection with `id`, `name`, and `status`.
- Requires `POST /api/v1/finance/transactions/sent` and `POST /api/v1/finance/transactions/received` to treat closed accounts as invalid write targets.
- Unknown accounts return `404`.
- Known but closed accounts remain readable and reject new transaction writes with `409`.
- Must preserve read compatibility for closed accounts through account listing filters.

## Technical Details
- Account status must be available to transaction write validation without replaying the full event stream on every request.
- Account closure should append an account lifecycle event rather than mutating prior account history.
- Closed-account enforcement should be centralized so income and expense write flows cannot diverge.
- The implementation should remain Postgres-only for finance persistence.

## Testing
- Closing an active account succeeds and returns closed status.
- Closing an already closed account succeeds and returns the unchanged closed state.
- Closing an unknown account returns `404`.
- Creating an expense on a closed account returns `409`.
- Creating income on a closed account returns `409`.
- Closed accounts remain visible through `status=closed` and `status=all` list queries.

## Rollout And Compatibility
- Backward-compatible because this is a new API surface.
- Any migration should be additive for account lifecycle events and projection state.
