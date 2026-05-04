CREATE TABLE IF NOT EXISTS finance_transaction_classification_events (
  event_id TEXT PRIMARY KEY,
  user_id TEXT NOT NULL,
  transaction_id TEXT NOT NULL,
  event_type TEXT NOT NULL CHECK (event_type IN ('TransactionCategorized', 'TransactionSplit')),
  category TEXT NULL,
  splits_payload JSONB NULL,
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS finance_transaction_categories (
  user_id TEXT NOT NULL,
  transaction_id TEXT NOT NULL,
  category TEXT NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (user_id, transaction_id)
);

CREATE TABLE IF NOT EXISTS finance_transaction_splits (
  user_id TEXT NOT NULL,
  transaction_id TEXT NOT NULL,
  split_index INTEGER NOT NULL,
  amount BIGINT NOT NULL CHECK (amount > 0),
  category TEXT NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (user_id, transaction_id, split_index)
);

CREATE INDEX IF NOT EXISTS finance_transaction_splits_user_transaction_idx
  ON finance_transaction_splits (user_id, transaction_id, split_index);
