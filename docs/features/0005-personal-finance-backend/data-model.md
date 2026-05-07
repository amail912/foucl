# Personal Finance Backend Data Model

## Goal
Summarize the draft domain model, event model, invariants, and projection expectations behind the personal finance backend backlog.

## Domain Entities
### Account
- `id`
- `name`
- `status`

Rules:
- `name` must not be empty or whitespace-only
- surrounding whitespace is trimmed before persistence and comparison
- `name` is unique per user using case-insensitive comparison across active and closed accounts
- stored statuses are `active` and `closed`

### Transaction
Derived from exactly one money event:
- `MoneySent`
- `MoneyReceived`

Rules:
- write requests require an `Idempotency-Key` header
- missing `Idempotency-Key` is a validation failure
- reusing an idempotency key with the same effective request returns the original success result
- reusing an idempotency key with a different effective request is a conflict
- `occurredAt` may be client-supplied or default to now
- `recordedAt` is assigned by the backend when the event is persisted

### ReconciliationAdjustment
Snapshot-scoped signed correction used to settle a balance discrepancy.

Rules:
- adjustments are first-class financial events, not ordinary money-entry transactions
- the backend derives the signed adjustment amount from the current snapshot discrepancy
- each snapshot has at most one current adjustment in projection state
- later adjustments supersede earlier adjustments for the same snapshot
- creating an adjustment auto-marks the target snapshot reconciled
- adjustment rows are distinguishable from ordinary transactions through an explicit marker in convenience views

### Category
Hierarchical classification used by categorization and split operations.

Rules:
- the effective category tree contains backend-owned built-in categories and user-owned categories
- each category has at most one parent in v1
- no category defaults exist in v1
- built-in categories are read-only through public write endpoints
- top-level built-in categories are non-selectable
- non-top-level categories are selectable even when they have children
- user-owned categories may be attached under built-in or user-owned parents
- built-in category names are explicit and self-describing without requiring path-only disambiguation

Canonical built-in hierarchy:

```text
Income
  Salary
  Freelance
  Reimbursement
  Gift
  Interest

Housing
  Rent / Mortgage
  Utilities
  Internet
  Home Insurance
  Maintenance
  Furniture

Food
  Groceries
  Restaurants
  Coffee / Snacks
  Delivery

Transport
  Public Transport
  Fuel
  Parking
  Taxi / Ride Share
  Vehicle Maintenance
  Vehicle Insurance

Health
  Doctor
  Pharmacy
  Health Insurance
  Therapy

Personal
  Clothing
  Education
  Books
  Subscriptions
  Digital Services

Household
  Cleaning Supplies
  Appliances
  Tools
  Home Goods

Pets
  Food
  Veterinary
  Toys
  Training

Leisure
  Entertainment
  Leisure Travel
  Hobbies
  Sports
  Games
  Garden

Family & Social
  Gifts
  Donations
  Childcare
  Events

Financial
  Bank Fees
  Taxes

Professional
  Equipment
  Software
  Professional Travel
  Professional Meals
  Training
  Services

Adjustments
  Refund

Uncategorized
  Uncategorized Expense
  Uncategorized Income
```

### TransactionSplit
Refines one transaction into multiple categorized parts.

Rule:
- splits do not create new transactions

### TransactionLink
Explicit relationship between two transactions.

Initial link type:
- `transfer`

Rules:
- transfer links are one-to-one in v1
- transfer links require opposite directions
- transfer links require equal amounts
- transfer links require distinct accounts
- no unlinking or transfer-link replacement exists in v1

### TransactionNote
Stable note lifecycle attached to a transaction.

Rules:
- note creation is append-only at the event level
- each note has a stable note id
- note text is free text only in v1
- note text is trimmed before persistence
- note text must be non-empty after validation trimming
- note text length is limited to 2000 characters after trimming
- note edits replace current text in current reads while preserving event history
- note delete is soft delete in v1
- deleted notes are omitted from the current transaction-row note list

### BalanceSnapshot
Observed account balance at a point in time.

Rules:
- snapshots are canonical persisted events
- snapshots are allowed on active and closed accounts
- duplicate account-plus-timestamp snapshots are invalid
- each snapshot has a one-to-one correspondence with one reconciliation result
- snapshots carry a persisted reconciliation status of `unreconciled` or `reconciled`
- a reconciled snapshot can serve as a trusted basis for later reconciliation reads on the same account
- when no reconciled basis exists, reconciliation falls back to transaction-derived behavior

## Event Envelope
Draft shape:

```json
{
  "id": "...",
  "event_type": "...",
  "event_version": 1,
  "occurred_at": "...",
  "recorded_at": "...",
  "actor_user_id": "...",
  "idempotency_key": "...",
  "payload": {}
}
```

## Draft Event Types
- `AccountOpened`
- `AccountClosed`
- `MoneySent`
- `MoneyReceived`
- `TransactionCategorized`
- `TransactionSplit`
- `TransactionLinked`
- `TransactionNoteAdded`
- `TransactionNoteUpdated`
- `TransactionNoteDeleted`
- `BalanceSnapshotRecorded`
- `BalanceSnapshotReconciliationStatusSet`
- `BalanceSnapshotAdjustmentRecorded`

## Core Invariants
### Money
- `amount > 0`
- integer values only

### Accounts
- account names are unique per user
- account-name uniqueness is case-insensitive and uses trimmed names
- closed accounts remain readable
- closed accounts cannot accept new transaction writes

### Transactions
- exactly one money event per transaction
- immutable after creation
- invalid create payloads reject with validation semantics
- idempotency-key reuse with a different effective request rejects with conflict semantics
- unknown accounts reject transaction writes with not-found semantics
- closed accounts reject transaction writes with conflict semantics

### Categories
- built-in and user-owned categories share one effective tree per user
- deleting a referenced user category is rejected
- categories referenced by active categorization or active split state must remain resolvable

### Classification State
- a whole-transaction category may be replaced by a later categorize event while no split is active
- active split state supersedes whole-transaction category state
- a later split replaces the current active split state
- whole-transaction categorize requests are rejected while a split is active

### Splits
- `sum(splits) == transaction amount`
- split payloads require at least two rows
- split rows may repeat category ids
- split rows refine one existing transaction only

### Transfers
- only defined through `TransactionLinked(link_type = transfer)`
- no automatic transfer detection
- unlinked transactions are treated as normal income or expense
- transfer-linked transactions remain visible in ledger reads
- transfer-linked transactions are excluded from report aggregates

### Notes
- current note reads expose only non-deleted notes
- note ordering in current reads follows original append order for surviving notes
- note edits do not change note ordering
- deleted note history remains preserved in raw events

### Snapshots
- reconciliation is computed from a snapshot plus transaction history, not stored as its own persisted object
- a selected reconciled snapshot becomes the balance basis at its timestamp
- later transactions adjust forward from the latest reconciled basis
- adjustment creation settles the selected snapshot and does not mutate historical transaction rows
- snapshot-write success responses are centered on the snapshot that was just created, even when it is backdated
- snapshots do not change report aggregates
- snapshot list may appear in convenience export views, but reconciliation does not
- snapshot list rows expose reconciliation status

### Export
- raw event history is canonical in exports
- `views` are convenience data only
- partial future import compatibility applies only to the canonical raw-event section
- delete or retention workflows are out of scope for v1 export behavior

### Event Log
- append-only
- idempotent writes

## Projection Expectations
### Accounts
- default account listing returns active accounts only
- `status=closed` returns only closed accounts
- `status=all` returns active and closed accounts

### Categories
- `GET /api/v1/finance/categories` returns the effective tree for the authenticated user
- category rows expose `id`, `name`, `parentId`, ownership summary, and `selectable`
- top-level built-in categories are non-selectable and all deeper categories are selectable
- built-in category ids are deterministic path-like slugs
- `Uncategorized` built-ins remain part of the category tree and also participate in report-side `uncategorized` filtering

### Transactions
- create-success responses and list rows share one common transaction-row shape
- transaction list ordering is `occurredAt` descending with transaction `id` as deterministic tie-breaker
- `GET /transactions` supports optional `accountId`, `from`, and `to` filters
- `from` is inclusive and `to` is exclusive
- no pagination in v1
- `transfer` is `null` or a full transfer summary object
- transfer summaries expose peer transaction id, peer account id, peer amount, and linked time
- `category` summary is populated only when no active split exists
- `splits` summary is populated when an active split exists
- `notes` exposes the current non-deleted note list with `id`, `text`, `createdAt`, and `updatedAt`

### Snapshots
- `GET /api/v1/finance/accounts/{id}/snapshots` returns snapshot discovery rows with `id`, `occurredAt`, `balance`, and `reconciliationStatus`
- snapshots are ordered by `occurredAt` descending with snapshot `id` as deterministic tie-breaker
- `GET /api/v1/finance/accounts/{id}/reconciliation` returns the latest reconciliation by default
- `POST /api/v1/finance/accounts/{id}/snapshots` returns reconciliation for the created snapshot rather than necessarily the latest snapshot on the account
- `POST /api/v1/finance/accounts/{id}/snapshots/{snapshotId}/adjustment` returns the updated reconciliation state and the current adjustment summary
- `GET /api/v1/finance/accounts/{id}/reconciliation?snapshotId=...` returns reconciliation for the selected snapshot
- reconciliation responses expose `snapshotId`, `snapshotOccurredAt`, `reconciliationStatus`, `basisSnapshotId`, `basisSnapshotOccurredAt`, `observedBalance`, `derivedBalanceAtSnapshot`, `discrepancy`, and `adjustment`

### Ledger
- includes all transactions, including transfers
- includes reconciliation adjustment rows with an explicit marker
- should remain auditable against underlying event history

### Reports
- `GET /report` is a pure aggregate endpoint
- report filters use required `from` and `to`, plus optional direction, account include/exclude, and category include/exclude lists
- include/exclude lists use repeated query parameters in v1
- overlapping include/exclude values for the same dimension are validation failures
- `direction=all` returns net balance delta with income positive and spending negative
- category filtering uses active split state when present and whole-transaction category only when no split is active
- when split-driven category filtering is active, aggregate totals sum only the matching split-row amounts
- reconciliation adjustment rows contribute to report totals by their signed amount
- reconciliation adjustment rows are treated as uncategorized for category matching
- the special category token `uncategorized` matches transactions with no active category state
- the special category token `uncategorized` also matches explicit `Uncategorized Expense` or `Uncategorized Income` assignments, including split rows
- response fields are `total`, `count`, and `transactionIds`
- `count` and `transactionIds` are row-based and include reconciliation adjustment rows
- `transactionIds` follow the same deterministic order as ledger rows

### Export
- `GET /export` returns one versioned JSON document
- top-level sections are `formatVersion`, canonical `events`, and convenience `views`
- `formatVersion` is `1` in the first release
- `events` use the canonical stored event-envelope representation
- `views` include current accounts, categories, transactions, transfer state, non-deleted notes, snapshot list with reconciliation status, and adjustment markers
- `views.transactions` reuse the `GET /transactions` row shape
- `views` exclude reconciliation outputs, aggregate report outputs, and deleted notes

## Known Open Questions
- correction-event strategy for mistakes in immutable history
- event versioning policy
