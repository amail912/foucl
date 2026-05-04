# Record Income And Expense Transactions

## Goal
As a user, I want to record income and expense entries manually, so my financial history is complete and auditable.

## Behavior And Business Rules
- `POST /api/v1/finance/transactions/sent` records one immutable `MoneySent` event.
- `POST /api/v1/finance/transactions/received` records one immutable `MoneyReceived` event.
- `amount` must be a positive integer for all money-entry writes and is expressed in cents.
- `occurredAt` is optional and defaults to now when omitted.
- Each transaction must have exactly one money event and cannot be mutated later into the opposite direction.
- Unknown accounts reject transaction creation with `404`.
- Closed accounts reject transaction creation with `409`.
- Client retries must use an `Idempotency-Key` request header so duplicate create requests do not append duplicate events.
- Missing `Idempotency-Key` is a validation failure.
- Reusing an `Idempotency-Key` with the same effective request returns the original success result.
- Reusing an `Idempotency-Key` with a different effective request is a conflict.

## Data And Contracts
- Defines `POST /api/v1/finance/transactions/sent` and `POST /api/v1/finance/transactions/received`.
- Request fields are `accountId`, `amount`, and optional `occurredAt`.
- Success responses return the created transaction row with `id`, `direction`, `accountId`, `amount`, `occurredAt`, `recordedAt`, `transfer`, `category`, `splits`, and `notes`.
- In this implementation slice, `transfer` and `category` are `null`, and `splits` and `notes` are empty until later stories add those behaviors.
- Must keep `404` for unknown accounts distinct from `409` for closed accounts.
- Missing `Idempotency-Key`, missing required fields, invalid timestamps, and invalid money amounts return `400`.
- Reusing an idempotency key with a different effective request returns `409`.

## Technical Details
- Transaction write paths should append one event per successful create command.
- Idempotent retries must be enforced through the request header before duplicate events can be appended.
- The idempotency check must compare the effective normalized request, not only the presence of the key.
- Closed-account checks and ownership checks must happen before event append.
- `recordedAt` must be assigned by the backend when the event is persisted.
- Success response rows should use the same transaction-row shape as `GET /transactions`.

## Testing
- Create expense transaction succeeds with valid input.
- Create income transaction succeeds with valid input.
- Omitting `occurredAt` succeeds and uses the backend default time.
- Missing `Idempotency-Key` returns `400`.
- Zero, negative, or non-integer amounts return `400`.
- Invalid `occurredAt` values return `400`.
- Retries with the same `Idempotency-Key` and the same effective request do not create duplicate transactions and return the original success result.
- Reusing the same `Idempotency-Key` with a different effective request returns `409`.
- Creating a transaction against an unknown account returns `404`.
- Creating a transaction against a closed account returns `409`.

## Rollout And Compatibility
- Backward-compatible because this is a new API surface.
- Observability should cover validation failures and duplicate-idempotency paths.
