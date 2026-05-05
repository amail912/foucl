DO $$
BEGIN
  IF to_regclass('public.finance_transaction_note_events') IS NULL THEN
    RETURN;
  END IF;

  DELETE FROM finance_transaction_note_events
  WHERE event_type IN ('TransactionNoteUpdated', 'TransactionNoteDeleted');

  UPDATE finance_transaction_note_events
  SET note_text = ''
  WHERE note_text IS NULL;

  ALTER TABLE finance_transaction_note_events
    ALTER COLUMN note_text SET NOT NULL;

  ALTER TABLE finance_transaction_note_events
    DROP CONSTRAINT IF EXISTS finance_transaction_note_events_event_type_check;

  ALTER TABLE finance_transaction_note_events
    ADD CONSTRAINT finance_transaction_note_events_event_type_check
    CHECK (event_type IN ('TransactionNoteAdded'));
END $$;
