CREATE TABLE IF NOT EXISTS finance_balance_snapshot_events (
  event_id TEXT PRIMARY KEY,
  user_id TEXT NOT NULL,
  account_id TEXT NOT NULL,
  snapshot_id TEXT NOT NULL,
  event_type TEXT NOT NULL CHECK (event_type IN ('BalanceSnapshotRecorded')),
  balance BIGINT NOT NULL,
  occurred_at TIMESTAMPTZ NOT NULL,
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS finance_balance_snapshots (
  user_id TEXT NOT NULL,
  account_id TEXT NOT NULL,
  snapshot_id TEXT NOT NULL,
  balance BIGINT NOT NULL,
  occurred_at TIMESTAMPTZ NOT NULL,
  recorded_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (user_id, account_id, snapshot_id),
  UNIQUE (user_id, account_id, occurred_at)
);

CREATE INDEX IF NOT EXISTS finance_balance_snapshots_user_account_occurred_idx
  ON finance_balance_snapshots (user_id, account_id, occurred_at DESC, snapshot_id ASC);
