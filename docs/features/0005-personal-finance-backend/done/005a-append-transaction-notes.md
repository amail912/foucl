# Append Transaction Notes

## Goal
As a user, I want to add notes to transactions, so I can preserve context alongside financial facts.

## Behavior And Business Rules
- Users can append notes to their own transactions.
- Note creation must not alter the underlying money facts, categorization, or transfer semantics of a transaction.
- Note text is free text only in v1.
- Note text must not be empty or whitespace-only after validation trimming.
- Note text length must not exceed 2000 characters.
- Appended notes appear in the transaction row's `notes` array.

## Data And Contracts
- Defines `POST /api/v1/finance/transactions/{id}/notes`.
- Request payload contains `text`.
- Blank or oversized note text returns `400`.
- Unknown or foreign-scope transactions return `404`.
- Success returns the updated transaction row.
- Note objects in the transaction row contain `id`, `text`, `createdAt`, and `updatedAt`.

## Technical Details
- The event stream should treat note creation as append-only with stable note ids.
- Projection design should support deterministic note ordering per transaction.
- Notes must remain excluded from financial calculations and report totals.
- Ownership and transaction existence checks must happen before event append.

## Testing
- Appending a note to an existing transaction succeeds.
- Appending a note with exactly 2000 characters succeeds.
- Appending a note that exceeds 2000 characters returns `400`.
- Appending a note with empty or whitespace-only text returns `400`.
- Appending a note to an unknown transaction returns `404`.
- Appending a note to a foreign-scope transaction returns `404`.
- The appended note appears in the returned transaction row and later ledger reads.

## Rollout And Compatibility
- Backward-compatible because this is a new API surface.
- Schema changes should be additive for note events and projections.
