ALTER TABLE finance_transaction_note_events
  DROP CONSTRAINT IF EXISTS finance_transaction_note_events_event_type_check;

ALTER TABLE finance_transaction_note_events
  ADD CONSTRAINT finance_transaction_note_events_event_type_check
  CHECK (event_type IN ('TransactionNoteAdded', 'TransactionNoteUpdated', 'TransactionNoteDeleted'));

ALTER TABLE finance_transaction_note_events
  ALTER COLUMN note_text DROP NOT NULL;
