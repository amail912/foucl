CREATE TABLE finance_events (
  event_number BIGSERIAL PRIMARY KEY,
  event_id TEXT NOT NULL UNIQUE,
  user_id TEXT NOT NULL,
  stream_id TEXT NOT NULL,
  stream_version BIGINT NOT NULL CHECK (stream_version > 0),
  event_type TEXT NOT NULL,
  event_version INTEGER NOT NULL CHECK (event_version > 0),
  occurred_at TIMESTAMPTZ NOT NULL,
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  idempotency_key TEXT,
  payload JSONB NOT NULL,
  UNIQUE (stream_id, stream_version)
);

CREATE INDEX finance_events_user_event_number_idx
  ON finance_events (user_id, event_number);

CREATE TABLE finance_account_events (
  event_id TEXT PRIMARY KEY,
  user_id TEXT NOT NULL,
  account_id TEXT NOT NULL,
  event_type TEXT NOT NULL,
  display_name TEXT NOT NULL,
  normalized_name TEXT NOT NULL,
  status TEXT NOT NULL CHECK (status IN ('active', 'closed')),
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE finance_accounts (
  user_id TEXT NOT NULL,
  account_id TEXT PRIMARY KEY,
  display_name TEXT NOT NULL,
  normalized_name TEXT NOT NULL,
  status TEXT NOT NULL CHECK (status IN ('active', 'closed')),
  opened_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (user_id, normalized_name)
);

CREATE INDEX finance_accounts_user_status_name_idx
  ON finance_accounts (user_id, status, display_name, account_id);
