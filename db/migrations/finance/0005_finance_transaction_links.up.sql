CREATE TABLE IF NOT EXISTS finance_transaction_link_events (
  event_id TEXT PRIMARY KEY,
  user_id TEXT NOT NULL,
  source_transaction_id TEXT NOT NULL,
  target_transaction_id TEXT NOT NULL,
  link_type TEXT NOT NULL CHECK (link_type IN ('transfer')),
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (source_transaction_id <> target_transaction_id)
);

CREATE TABLE IF NOT EXISTS finance_transaction_links (
  user_id TEXT NOT NULL,
  transaction_id TEXT NOT NULL,
  peer_transaction_id TEXT NOT NULL,
  link_type TEXT NOT NULL CHECK (link_type IN ('transfer')),
  linked_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (user_id, transaction_id),
  CHECK (transaction_id <> peer_transaction_id)
);

CREATE INDEX IF NOT EXISTS finance_transaction_links_user_peer_idx
  ON finance_transaction_links (user_id, peer_transaction_id);
