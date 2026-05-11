# Transaction Metadata Update Endpoint

## Goal
As a user, I want to edit transaction metadata after creation, so I can correct or enrich counterparty and description details.

## Behavior And Business Rules
- Add endpoint:
  - `POST /api/v1/finance/transactions/{id}/metadata`
- Request supports partial metadata mutation:
  - `counterparty?: string | null`
  - `description?: string | null`
- At least one metadata field must be provided.
- Input normalization:
  - `counterparty` is trimmed, lowercased, and empty string after trim is normalized to `null`.
  - `description` is trimmed and empty string after trim is normalized to `null`.
- Validation:
  - `counterparty` max length is `120` characters.
  - `description` max length is `1000` characters.
  - Provided metadata values must be string or `null`.
  - Requests that provide neither metadata field return `400`.
  - Validation failures return `400`.

## Data And Contracts
- Success returns the updated transaction payload using the same transaction shape as ledger reads.
- Omitted metadata fields preserve their current values.
- Provided metadata fields replace their current values after normalization.
- Unknown transaction id returns `404`.
- Reconciliation adjustment rows are not mutable through this endpoint and return `404`.
- Auth and ownership rules follow existing finance transaction mutation patterns.

## Technical Details
- Implement metadata updates through canonical finance write flows so change history remains auditable.
- Update read projections deterministically so metadata changes appear in subsequent reads.
- Keep non-metadata transaction fields immutable and unchanged by this endpoint.
- Reuse metadata normalization and validation rules from story `012`.

## Testing
- Metadata update succeeds with both fields provided.
- Metadata update succeeds when one field is omitted and one is present; omitted field is preserved.
- Metadata update succeeds when values normalize to `null`.
- Counterparty values are lowercased on update.
- Empty request body or request with neither metadata field returns `400`.
- Non-string metadata values return `400`.
- Over-length values return `400`.
- Unknown transaction id returns `404`.
- Metadata update against reconciliation adjustment row id returns `404`.
- Updated transaction is reflected in `GET /api/v1/finance/transactions`.

## Rollout And Compatibility
- v1 contract definition for transaction metadata mutation under `/api/v1/finance`.
