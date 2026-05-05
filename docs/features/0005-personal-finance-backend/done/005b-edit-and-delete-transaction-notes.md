# Edit And Delete Transaction Notes

## Goal
As a user, I want to correct or remove transaction notes, so current transaction context stays useful without losing auditability.

## Behavior And Business Rules
- Users can edit an existing transaction note.
- Users can soft-delete an existing transaction note.
- Editing a note replaces its current text in current reads.
- Soft-deleted notes are hidden from the current `notes` array.
- Audit history remains preserved in event history.
- Note edits and deletes must not alter money facts, categorization, or transfer semantics.
- Note text is trimmed before persistence for append and update writes.
- Updated note text must not be empty or whitespace-only after validation trimming.
- Updated note text length must not exceed 2000 characters.

## Data And Contracts
- Defines `PUT /api/v1/finance/transactions/{transactionId}/notes/{noteId}` and `DELETE /api/v1/finance/transactions/{transactionId}/notes/{noteId}`.
- The update request payload contains `text`.
- Blank or oversized updated note text returns `400`.
- Unknown or foreign-scope transaction or note ids return `404`.
- Update and delete success responses return the updated transaction row.
- Current transaction rows expose only non-deleted notes, each with `id`, `text`, `createdAt`, and `updatedAt`.
- Transaction create requests do not embed notes in this iteration; note text normalization rules apply to note append and note update endpoints.

## Technical Details
- Notes must use stable note ids so edit and delete target one logical note.
- Soft delete must preserve audit history rather than physically removing note events.
- Current-state projection must resolve the latest note text and omit deleted notes.
- Note ordering in current reads should remain based on original append order.

## Testing
- Editing an existing note succeeds.
- Deleting an existing note succeeds.
- Editing a note updates its text and `updatedAt` in current transaction rows.
- Editing a note with empty or whitespace-only text returns `400`.
- Editing a note with more than 2000 characters returns `400`.
- Deleting a note removes it from current transaction rows.
- Editing or deleting an unknown note returns `404`.
- Editing or deleting a foreign-scope note returns `404`.

## Rollout And Compatibility
- Backward-compatible because this is a new API surface.
- Schema and projection changes for note lifecycle should be additive.
