# Link Transfer Transactions

## Goal
As a user, I want to explicitly mark transfers between my accounts, so internal money movement does not distort aggregate report totals.

## Behavior And Business Rules
- A transfer exists only when two transactions are explicitly linked with `link_type = transfer`.
- Unlinked transactions remain ordinary income or expense entries.
- Valid transfer links require opposite directions, equal amounts, distinct accounts, and one-to-one pairing.
- Transactions already participating in a transfer link cannot be linked again in v1.
- Same-account transaction pairs are invalid for transfer linking.
- Transfer-linked transactions are excluded from `GET /report` aggregates.
- Transfer-linked transactions remain visible in ledger and balance-oriented views.
- No unlink operation exists in v1.

## Data And Contracts
- Defines `POST /api/v1/finance/transactions/link` for explicit transaction relationships.
- Request fields are `sourceTransactionId`, `targetTransactionId`, and `linkType`.
- `linkType = transfer` is the only supported value in v1.
- Unknown or foreign-scope transactions return `404`.
- Already linked transactions return `409`.
- Same-account pairs, same-direction pairs, different-amount pairs, and other invalid transfer matches return `409`.
- Success responses return an object with both updated transaction rows: `source` and `target`.
- Transaction rows expose `transfer` as `null` or a full transfer object with `linkType`, `peerTransactionId`, `peerAccountId`, `peerAmount`, and `linkedAt`.

## Technical Details
- Linking must be atomic so the system cannot persist one-sided transfer state.
- Projection updates must reclassify affected transactions in reports after a successful link.
- Concurrency behavior for simultaneous link attempts should be deterministic.
- Link validation should remain scoped to the authenticated user's transactions only.
- Transfer summaries should be derived from current link state rather than inferred heuristically from amounts or timestamps.

## Testing
- Linking a valid opposite-direction, equal-amount, distinct-account pair as a transfer succeeds.
- Successful transfer linking returns both updated transaction rows with populated transfer summaries.
- `GET /report` excludes transfer-linked transactions from aggregate totals.
- Ledger views continue to include transfer-linked transactions with populated transfer summaries.
- Linking transactions that are already linked returns `409`.
- Linking transactions with incompatible directions, amounts, or accounts returns `409`.
- Linking unknown or foreign-scope transactions returns `404`.

## Rollout And Compatibility
- Backward-compatible because this is a new API surface.
- Report projections must remain consistent when previously reported transactions become transfer-linked later.
