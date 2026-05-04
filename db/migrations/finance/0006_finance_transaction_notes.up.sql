CREATE TABLE IF NOT EXISTS finance_transaction_note_events (
  event_id TEXT PRIMARY KEY,
  user_id TEXT NOT NULL,
  transaction_id TEXT NOT NULL,
  note_id TEXT NOT NULL,
  event_type TEXT NOT NULL CHECK (event_type IN ('TransactionNoteAdded')),
  note_text TEXT NOT NULL,
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS finance_transaction_notes (
  user_id TEXT NOT NULL,
  transaction_id TEXT NOT NULL,
  note_id TEXT NOT NULL,
  note_text TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (user_id, transaction_id, note_id)
);

CREATE INDEX IF NOT EXISTS finance_transaction_notes_user_transaction_created_idx
  ON finance_transaction_notes (user_id, transaction_id, created_at, note_id);
