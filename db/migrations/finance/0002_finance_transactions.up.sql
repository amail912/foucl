CREATE TABLE finance_transaction_events (
  event_id TEXT PRIMARY KEY,
  user_id TEXT NOT NULL,
  transaction_id TEXT NOT NULL,
  account_id TEXT NOT NULL,
  direction TEXT NOT NULL CHECK (direction IN ('sent', 'received')),
  amount BIGINT NOT NULL CHECK (amount > 0),
  occurred_at TIMESTAMPTZ NOT NULL,
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE finance_transactions (
  user_id TEXT NOT NULL,
  transaction_id TEXT PRIMARY KEY,
  account_id TEXT NOT NULL,
  direction TEXT NOT NULL CHECK (direction IN ('sent', 'received')),
  amount BIGINT NOT NULL CHECK (amount > 0),
  occurred_at TIMESTAMPTZ NOT NULL,
  recorded_at TIMESTAMPTZ NOT NULL
);

CREATE TABLE finance_transaction_idempotency (
  user_id TEXT NOT NULL,
  idempotency_key TEXT NOT NULL,
  direction TEXT NOT NULL CHECK (direction IN ('sent', 'received')),
  account_id TEXT NOT NULL,
  amount BIGINT NOT NULL CHECK (amount > 0),
  occurred_at_supplied BOOLEAN NOT NULL,
  occurred_at TIMESTAMPTZ NOT NULL,
  transaction_id TEXT NOT NULL,
  PRIMARY KEY (user_id, idempotency_key)
);

CREATE INDEX finance_transactions_user_occurred_idx
  ON finance_transactions (user_id, occurred_at DESC, transaction_id);

CREATE INDEX finance_transactions_user_account_occurred_idx
  ON finance_transactions (user_id, account_id, occurred_at DESC, transaction_id);
