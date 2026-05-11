# Rich Transaction Metadata Fields And Write Support

## Goal
As a user, I want to store counterparty and free-form description metadata when recording transactions, so ledger entries carry the contextual details the frontend needs.

## Behavior And Business Rules
- Add optional transaction metadata fields to the finance transaction read model:
  - `counterparty: string | null`
  - `description: string | null`
- New metadata fields appear on all finance endpoints that return transaction objects, including `GET /api/v1/finance/transactions` and create responses.
- Metadata fields are always present in the transaction row shape.
- Reconciliation adjustment rows include `counterparty` and `description` with `null` values.
- Extend transaction-create endpoints with optional metadata:
  - `POST /api/v1/finance/transactions/sent`
  - `POST /api/v1/finance/transactions/received`
- Input normalization:
  - `counterparty` is trimmed, lowercased, and empty string after trim is normalized to `null`.
  - `description` is trimmed and empty string after trim is normalized to `null`.
- Validation:
  - `counterparty` max length is `120` characters.
  - `description` max length is `1000` characters.
  - `counterparty` and `description` must be string or `null` when present.
  - Validation failures return `400`.

## Data And Contracts
- Request additions for both create endpoints:
  - `counterparty?: string | null`
  - `description?: string | null`
- This story defines the v1 metadata contract for create and read transaction surfaces.
- Existing non-metadata fields and semantics remain unchanged.

## Technical Details
- Persist metadata as first-class transaction state available to projection-backed reads.
- Ensure canonical write paths keep idempotency semantics unchanged when metadata is present.
- Idempotency effective-request comparisons include normalized metadata fields.
- Keep existing repository/domain error mapping unchanged for non-metadata behavior.
- Keep post-create metadata mutation out of scope for this story; it is covered by story `013`.
- Ensure story `015` search behavior can rely on persisted and readable `counterparty` and `description` values.

## Testing
- Create sent transaction succeeds with valid `counterparty` and `description`.
- Create received transaction succeeds with explicit `null` metadata.
- Metadata is trimmed on create and empty strings become `null`.
- Counterparty values are lowercased on create.
- Non-string metadata values return `400`.
- Over-length `counterparty` returns `400`.
- Over-length `description` returns `400`.
- Idempotent retries with the same normalized metadata return the original success result.
- Idempotency-key reuse with different normalized metadata returns `409`.
- Transaction-list responses include metadata fields for rows with and without metadata.
- Reconciliation adjustment rows include metadata fields with `null` values.
- Existing create flows without metadata remain unchanged.

## Rollout And Compatibility
- v1 contract definition for `/api/v1/finance` transaction metadata fields.
