# Personal Finance Backend API Contract

## Goal
Define the backend contract to support authenticated single-user personal finance tracking with immutable financial history and projection-backed queries.

## Scope
This contract note captures the intended public API surface for the first iteration and the settled v1 behavior defined by the ready backlog.

## Authentication And Scope Rules
- All finance endpoints require authentication.
- Every finance read and write is implicitly scoped to the authenticated user.
- The backend does not expose multi-user collaboration or shared finance resources in this iteration.
- Finance routes are mounted under `/api/v1/finance` at runtime.

## Accounts
### `POST /api/v1/finance/accounts`
Creates a finance account.

Request fields:
- `name`

Response fields:
- `id`
- `name`
- `status`

Rules:
- v1 accounts do not have a type field.
- account names must not be empty or whitespace-only.
- surrounding whitespace is trimmed before persistence and comparison.
- account names must be unique per authenticated user using case-insensitive comparison.
- uniqueness applies across both active and closed accounts.
- invalid account names return `400`.
- duplicate account names return `409`.
- success returns the current account projection state.

### `GET /api/v1/finance/accounts`
Returns the authenticated user's finance accounts.

Query parameters:
- optional `status`
  - `active` returns active accounts only
  - `closed` returns closed accounts only
  - `all` returns active and closed accounts

Default behavior:
- when `status` is omitted, return active accounts only

Response fields per account:
- `id`
- `name`
- `status`

### `POST /api/v1/finance/accounts/{id}/close`
Closes an account.

Rules:
- success returns the updated account projection state
- closing an already closed account is idempotent success
- closing an unknown account returns `404`
- closed accounts remain readable through `GET /accounts?status=closed` and `GET /accounts?status=all`

## Categories
### `GET /api/v1/finance/categories`
Returns the effective category tree for the authenticated user.

Response fields per category:
- `id`
- `name`
- `parentId`
- `owner`
- `selectable`

Rules:
- the effective tree includes backend-owned built-in categories and user-owned categories
- each category has at most one parent in v1
- built-in categories are read-only
- built-in category ids use deterministic slug ids, for example `income.salary` and `uncategorized.expense`
- built-in top-level categories are non-selectable
- non-top-level categories are selectable even when they have children
- built-in category names are explicit and self-describing without requiring path-only disambiguation

### `POST /api/v1/finance/categories`
Creates a user-owned category.

Request fields:
- `name`
- optional `parentId`

Rules:
- user-owned categories may be created with no parent or under a built-in or user-owned parent
- invalid parent ids, cross-user parent references, and attempted parent cycles return `400`

### `POST /api/v1/finance/categories/{id}`
Updates a user-owned category.

Request fields:
- `name`
- optional `parentId`

Rules:
- user-owned categories may be moved under a built-in or user-owned parent
- invalid parent ids, cross-user parent references, and attempted parent cycles return `400`
- built-in categories cannot be updated and return `409`

### `DELETE /api/v1/finance/categories/{id}`
Deletes a user-owned category.

Rules:
- deleting a category that is still referenced by current transaction categorization or active split state returns `409`
- deleting a category that still has child categories returns `409`
- built-in categories cannot be updated or deleted through public write endpoints
- deleting a built-in category returns `409`
- no category defaults exist in v1
- no built-in transfer category exists in v1

## Transactions
### Transaction Row Shape
The v1 transaction row shape used by create-success responses and `GET /api/v1/finance/transactions` is:
- `id`
- `direction`
- `accountId`
- `amount`
- `occurredAt`
- `recordedAt`
- `transfer`
- `category`
- `splits`
- `notes`

### `POST /api/v1/finance/transactions/sent`
Records one immutable expense transaction.

Request fields:
- `accountId`
- `amount` (in cents)
- optional `occurredAt`

Notes:
- transaction create payloads do not embed note text in v1; note writes use dedicated note endpoints

### `POST /api/v1/finance/transactions/received`
Records one immutable income transaction.

Request fields:
- `accountId`
- `amount` (in cents)
- optional `occurredAt`

Shared transaction write rules:
- `amount` must be a positive integer
- each request creates exactly one money event
- `occurredAt` defaults to now when omitted
- client retries must supply an `Idempotency-Key` request header
- missing `Idempotency-Key` returns `400`
- repeated create requests with the same idempotency key and the same effective request must return the original success result without appending a duplicate event
- repeated create requests with the same idempotency key and a different effective request return `409`
- malformed request bodies, invalid timestamps, invalid amounts, and missing required fields return `400`
- unknown target accounts return `404`
- new transactions cannot be created against closed accounts and return `409`
- success responses return the created transaction row

### `GET /api/v1/finance/transactions`
Returns projection-backed transaction history.

Query parameters:
- optional `accountId`
- optional `from`
- optional `to`

Default behavior:
- no pagination in v1
- transactions are ordered by `occurredAt` descending with transaction `id` as deterministic tie-breaker
- `from` filters on `occurredAt` inclusively
- `to` filters on `occurredAt` exclusively

Response fields per row:
- the v1 transaction row shape listed above

Read rules:
- `transfer` is `null` when the transaction is not transfer-linked
- `transfer` returns a full transfer object when linked, containing:
  - `linkType`
  - `peerTransactionId`
  - `peerAccountId`
  - `peerAmount`
  - `linkedAt`
- `category` returns the active whole-transaction category summary only when no split is active
- `splits` returns the active split summary when present
- `notes` returns the current append-ordered non-deleted note list for the transaction
- each note object in `notes` contains:
  - `id`
  - `text`
  - `createdAt`
  - `updatedAt`
- unknown or foreign-scope `accountId` filters return an empty list
- `from == to` returns an empty list
- `from > to` returns `400`

## Snapshots And Reconciliation
### `POST /api/v1/finance/accounts/{id}/snapshots`
Records an observed balance snapshot for an account.

Request fields:
- `balance`
- `occurredAt`

Rules:
- `balance` uses integer cents
- snapshots are allowed on active and closed accounts
- recording a snapshot does not rewrite transaction history
- duplicate snapshots for the same account and exact `occurredAt` return `409`
- unknown accounts return `404`
- success returns the reconciliation state for the snapshot that was just created, including its reconciliation status and selected basis metadata

### `PUT /api/v1/finance/accounts/{id}/snapshots/{snapshotId}/reconciliation-status`
Marks or unmarks a snapshot as reconciled.

Request fields:
- `status`

Allowed values:
- `unreconciled`
- `reconciled`

Rules:
- snapshot reconciliation status changes are idempotent
- unknown accounts or snapshots return `404`
- invalid status values return `400`
- success returns the updated snapshot reconciliation state

### `GET /api/v1/finance/accounts/{id}/snapshots`
Returns snapshot discovery data for one account.

Default behavior:
- snapshots are ordered by `occurredAt` descending with snapshot `id` as deterministic tie-breaker

Response fields per snapshot:
- `id`
- `occurredAt`
- `balance`
- `reconciliationStatus`

### `GET /api/v1/finance/accounts/{id}/reconciliation`
Returns reconciliation for one account.

Query parameters:
- optional `snapshotId`

Default behavior:
- when `snapshotId` is omitted, return reconciliation for the latest snapshot
- when `snapshotId` is present, return reconciliation for that specific snapshot

Response fields:
- `snapshotId`
- `snapshotOccurredAt`
- `basisSnapshotId`
- `basisSnapshotOccurredAt`
- `observedBalance`
- `derivedBalanceAtSnapshot`
- `discrepancy`

Rules:
- reconciliation is computed from the selected snapshot and transaction history, not stored as its own persisted object
- the selected snapshot becomes the balance basis when its status is `reconciled`
- later reconciled snapshots replace earlier reconciled bases for later target timestamps
- later transactions adjust forward from the selected reconciled basis
- when no reconciled basis exists, reconciliation falls back to the current transaction-derived baseline behavior
- unknown accounts or snapshot ids return `404`

## Operations
### `POST /api/v1/finance/transactions/{id}/categorize`
Assigns a whole-transaction category to a transaction.

Request fields:
- `category`

Rules:
- unknown transactions return `404`
- unknown category slugs return `404`
- non-selectable categories return `400`
- categorize requests against transactions with an active split return `409`
- when no split is active, a later categorize request replaces the current whole-transaction category state
- success returns the current transaction row

### `POST /api/v1/finance/transactions/{id}/split`
Applies categorized split rows to a transaction.

Request fields:
- `splits`

Split row fields:
- `category`
- `amount`

Rules:
- split amounts must sum to the original transaction amount
- split payloads must contain at least two rows
- all split category slugs must resolve in the effective category tree
- all split category slugs must be selectable
- repeated category slugs within one split payload are allowed
- a split supersedes any existing whole-transaction category in current state
- a later split replaces the current active split state
- success returns the current transaction row

### `POST /api/v1/finance/transactions/link`
Creates an explicit relationship between transactions.

Request fields:
- `sourceTransactionId`
- `targetTransactionId`
- `linkType`

Rules:
- `linkType = transfer` is the only supported v1 value
- a valid transfer link requires opposite directions, equal amounts, distinct accounts, and one-to-one pairing
- if either transaction is unknown or outside the authenticated user's scope, return `404`
- if either transaction is already transfer-linked, return `409`
- if the requested link violates transfer matching rules, return `409`
- no unlink endpoint exists in v1
- success returns an object with `source` and `target` updated transaction rows

### `POST /api/v1/finance/transactions/{id}/notes`
Appends a note to a transaction.

Request fields:
- `text`

Rules:
- note text is free text only in v1
- note text is trimmed before persistence
- note text must not be empty or whitespace-only after validation trimming
- note text length must not exceed 2000 characters after trimming
- blank or oversized note text returns `400`
- success returns the updated transaction row

### `PUT /api/v1/finance/transactions/{transactionId}/notes/{noteId}`
Updates one existing transaction note.

Request fields:
- `text`

Rules:
- updates replace the note's current text in current reads
- note text is trimmed before persistence
- note text must not be empty or whitespace-only after validation trimming
- note text length must not exceed 2000 characters after trimming
- blank or oversized note text returns `400`
- unknown or foreign-scope transaction or note ids return `404`
- success returns the updated transaction row

### `DELETE /api/v1/finance/transactions/{transactionId}/notes/{noteId}`
Soft-deletes one existing transaction note.

Rules:
- deleted notes are hidden from the current `notes` array
- unknown or foreign-scope transaction or note ids return `404`
- success returns the updated transaction row

## Reports
### `GET /report`
Returns aggregate values for the filtered transaction set.

Query parameters:
- required `from`
- required `to`
- optional `direction=sent|received|all`
- optional `accountIn`
- optional `accountNotIn`
- optional `categoryIn`
- optional `categoryNotIn`

Encoding rules:
- `accountIn`, `accountNotIn`, `categoryIn`, and `categoryNotIn` use repeated query parameters in v1
- repeated-parameter examples include `accountIn=a&accountIn=b` and `categoryNotIn=x`

Filter rules:
- `direction=sent` aggregates only expense transactions
- `direction=received` aggregates only income transactions
- `direction=all` aggregates net balance delta with income positive and spending negative
- transfer-linked transactions are excluded from all report aggregates
- overlapping include and exclude values for the same filter dimension return `400`
- category filters apply to active split categories when a split is active
- whole-transaction category is ignored while a split is active
- category filters may include the special token `uncategorized`
- `uncategorized` matches transactions with no active category state
- `uncategorized` also matches transactions explicitly assigned `Uncategorized Expense` or `Uncategorized Income`
- `uncategorized` also matches split rows assigned to `Uncategorized Expense` or `Uncategorized Income`

Response fields:
- `total`
- `count`
- `transactionIds`

Response rules:
- `total` uses integer cents
- when a split is active and category filters are applied, `total` sums only the matching split-row amounts
- `count` is the number of transactions included in the aggregate
- `transactionIds` contains the ids of the transactions included in the aggregate
- transactions matched through multiple split rows still contribute once to `count` and appear once in `transactionIds`
- `transactionIds` follow the same deterministic order as the ledger: `occurredAt` descending with transaction `id` as tie-breaker

## Export
### `GET /export`
Exports the authenticated user's finance data as one versioned JSON document.

Top-level fields:
- `formatVersion`
- `events`
- `views`

Rules:
- `formatVersion = 1` in the first release
- raw event history in `events` is the canonical representation
- `events` use the canonical stored event-envelope representation
- `views` are convenience data only
- export is partially future-import-compatible only for the canonical raw-event section
- unauthenticated requests return `401`
- export generation failures return `5xx`

View contents:
- current accounts
- current categories
- current transactions using the same row shape as `GET /transactions`
- current transfer state
- current non-deleted notes
- snapshot list

View exclusions:
- reconciliation outputs
- aggregate report outputs
- deleted notes

## Status Semantics
Draft minimum meanings for finance endpoints:
- `401` unauthenticated
- `403` authenticated but not allowed for the operation
- `404` unknown account, transaction, category, note, snapshot, or route
- `409` valid request blocked by current resource state or invariant conflict
- `5xx` server or storage failure

Validation failures may use `400` when request bodies are malformed or semantically invalid, but story refinement should make that explicit per endpoint family.

## Contract Defaults To Preserve
- Money is represented in integer cents only.
- Transactions are immutable after their money-entry event is recorded.
- Splits do not create new transactions.
- The event log is append-only.
- Event writes must be idempotent for client retries.
