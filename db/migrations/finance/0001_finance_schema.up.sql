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

CREATE TABLE finance_categories (
  category_id TEXT PRIMARY KEY,
  user_id TEXT NULL,
  name TEXT NOT NULL,
  parent_id TEXT NULL REFERENCES finance_categories(category_id),
  owner TEXT NOT NULL CHECK (owner IN ('built_in', 'user')),
  selectable BOOLEAN NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (
    (owner = 'built_in' AND user_id IS NULL)
    OR (owner = 'user' AND user_id IS NOT NULL)
  )
);

CREATE INDEX finance_categories_user_parent_name_idx
  ON finance_categories (user_id, parent_id, name, category_id);

CREATE INDEX finance_categories_parent_idx
  ON finance_categories (parent_id, category_id);

INSERT INTO finance_categories (category_id, user_id, name, parent_id, owner, selectable) VALUES
  ('income', NULL, 'Income', NULL, 'built_in', FALSE),
  ('income.salary', NULL, 'Salary', 'income', 'built_in', TRUE),
  ('income.freelance', NULL, 'Freelance', 'income', 'built_in', TRUE),
  ('income.reimbursement', NULL, 'Reimbursement', 'income', 'built_in', TRUE),
  ('income.gift', NULL, 'Gift', 'income', 'built_in', TRUE),
  ('income.interest', NULL, 'Interest', 'income', 'built_in', TRUE),
  ('housing', NULL, 'Housing', NULL, 'built_in', FALSE),
  ('housing.rent-mortgage', NULL, 'Rent / Mortgage', 'housing', 'built_in', TRUE),
  ('housing.utilities', NULL, 'Utilities', 'housing', 'built_in', TRUE),
  ('housing.internet', NULL, 'Internet', 'housing', 'built_in', TRUE),
  ('housing.home-insurance', NULL, 'Home Insurance', 'housing', 'built_in', TRUE),
  ('housing.maintenance', NULL, 'Maintenance', 'housing', 'built_in', TRUE),
  ('housing.furniture', NULL, 'Furniture', 'housing', 'built_in', TRUE),
  ('food', NULL, 'Food', NULL, 'built_in', FALSE),
  ('food.groceries', NULL, 'Groceries', 'food', 'built_in', TRUE),
  ('food.restaurants', NULL, 'Restaurants', 'food', 'built_in', TRUE),
  ('food.coffee-snacks', NULL, 'Coffee / Snacks', 'food', 'built_in', TRUE),
  ('food.delivery', NULL, 'Delivery', 'food', 'built_in', TRUE),
  ('transport', NULL, 'Transport', NULL, 'built_in', FALSE),
  ('transport.public-transport', NULL, 'Public Transport', 'transport', 'built_in', TRUE),
  ('transport.fuel', NULL, 'Fuel', 'transport', 'built_in', TRUE),
  ('transport.parking', NULL, 'Parking', 'transport', 'built_in', TRUE),
  ('transport.taxi-ride-share', NULL, 'Taxi / Ride Share', 'transport', 'built_in', TRUE),
  ('transport.vehicle-maintenance', NULL, 'Vehicle Maintenance', 'transport', 'built_in', TRUE),
  ('transport.vehicle-insurance', NULL, 'Vehicle Insurance', 'transport', 'built_in', TRUE),
  ('health', NULL, 'Health', NULL, 'built_in', FALSE),
  ('health.doctor', NULL, 'Doctor', 'health', 'built_in', TRUE),
  ('health.pharmacy', NULL, 'Pharmacy', 'health', 'built_in', TRUE),
  ('health.health-insurance', NULL, 'Health Insurance', 'health', 'built_in', TRUE),
  ('health.therapy', NULL, 'Therapy', 'health', 'built_in', TRUE),
  ('personal', NULL, 'Personal', NULL, 'built_in', FALSE),
  ('personal.clothing', NULL, 'Clothing', 'personal', 'built_in', TRUE),
  ('personal.education', NULL, 'Education', 'personal', 'built_in', TRUE),
  ('personal.books', NULL, 'Books', 'personal', 'built_in', TRUE),
  ('personal.subscriptions', NULL, 'Subscriptions', 'personal', 'built_in', TRUE),
  ('personal.digital-services', NULL, 'Digital Services', 'personal', 'built_in', TRUE),
  ('household', NULL, 'Household', NULL, 'built_in', FALSE),
  ('household.cleaning-supplies', NULL, 'Cleaning Supplies', 'household', 'built_in', TRUE),
  ('household.appliances', NULL, 'Appliances', 'household', 'built_in', TRUE),
  ('household.tools', NULL, 'Tools', 'household', 'built_in', TRUE),
  ('household.home-goods', NULL, 'Home Goods', 'household', 'built_in', TRUE),
  ('pets', NULL, 'Pets', NULL, 'built_in', FALSE),
  ('pets.food', NULL, 'Food', 'pets', 'built_in', TRUE),
  ('pets.veterinary', NULL, 'Veterinary', 'pets', 'built_in', TRUE),
  ('pets.toys', NULL, 'Toys', 'pets', 'built_in', TRUE),
  ('pets.training', NULL, 'Training', 'pets', 'built_in', TRUE),
  ('leisure', NULL, 'Leisure', NULL, 'built_in', FALSE),
  ('leisure.entertainment', NULL, 'Entertainment', 'leisure', 'built_in', TRUE),
  ('leisure.travel', NULL, 'Leisure Travel', 'leisure', 'built_in', TRUE),
  ('leisure.hobbies', NULL, 'Hobbies', 'leisure', 'built_in', TRUE),
  ('leisure.sports', NULL, 'Sports', 'leisure', 'built_in', TRUE),
  ('leisure.games', NULL, 'Games', 'leisure', 'built_in', TRUE),
  ('leisure.garden', NULL, 'Garden', 'leisure', 'built_in', TRUE),
  ('family-social', NULL, 'Family & Social', NULL, 'built_in', FALSE),
  ('family-social.gifts', NULL, 'Gifts', 'family-social', 'built_in', TRUE),
  ('family-social.donations', NULL, 'Donations', 'family-social', 'built_in', TRUE),
  ('family-social.childcare', NULL, 'Childcare', 'family-social', 'built_in', TRUE),
  ('family-social.events', NULL, 'Events', 'family-social', 'built_in', TRUE),
  ('financial', NULL, 'Financial', NULL, 'built_in', FALSE),
  ('financial.bank-fees', NULL, 'Bank Fees', 'financial', 'built_in', TRUE),
  ('financial.taxes', NULL, 'Taxes', 'financial', 'built_in', TRUE),
  ('professional', NULL, 'Professional', NULL, 'built_in', FALSE),
  ('professional.equipment', NULL, 'Equipment', 'professional', 'built_in', TRUE),
  ('professional.software', NULL, 'Software', 'professional', 'built_in', TRUE),
  ('professional.travel', NULL, 'Professional Travel', 'professional', 'built_in', TRUE),
  ('professional.meals', NULL, 'Professional Meals', 'professional', 'built_in', TRUE),
  ('professional.training', NULL, 'Training', 'professional', 'built_in', TRUE),
  ('professional.services', NULL, 'Services', 'professional', 'built_in', TRUE),
  ('adjustments', NULL, 'Adjustments', NULL, 'built_in', FALSE),
  ('adjustments.refund', NULL, 'Refund', 'adjustments', 'built_in', TRUE),
  ('uncategorized', NULL, 'Uncategorized', NULL, 'built_in', FALSE),
  ('uncategorized.expense', NULL, 'Uncategorized Expense', 'uncategorized', 'built_in', TRUE),
  ('uncategorized.income', NULL, 'Uncategorized Income', 'uncategorized', 'built_in', TRUE);

CREATE TABLE finance_transaction_categories (
  user_id TEXT NOT NULL,
  transaction_id TEXT NOT NULL,
  category TEXT NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (user_id, transaction_id)
);

CREATE TABLE finance_transaction_splits (
  user_id TEXT NOT NULL,
  transaction_id TEXT NOT NULL,
  split_index INTEGER NOT NULL,
  amount BIGINT NOT NULL CHECK (amount > 0),
  category TEXT NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (user_id, transaction_id, split_index)
);

CREATE INDEX finance_transaction_splits_user_transaction_idx
  ON finance_transaction_splits (user_id, transaction_id, split_index);

CREATE TABLE finance_transaction_links (
  user_id TEXT NOT NULL,
  transaction_id TEXT NOT NULL,
  peer_transaction_id TEXT NOT NULL,
  link_type TEXT NOT NULL CHECK (link_type IN ('transfer')),
  linked_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (user_id, transaction_id),
  CHECK (transaction_id <> peer_transaction_id)
);

CREATE INDEX finance_transaction_links_user_peer_idx
  ON finance_transaction_links (user_id, peer_transaction_id);

CREATE TABLE finance_transaction_notes (
  user_id TEXT NOT NULL,
  transaction_id TEXT NOT NULL,
  note_id TEXT NOT NULL,
  note_text TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (user_id, transaction_id, note_id)
);

CREATE INDEX finance_transaction_notes_user_transaction_created_idx
  ON finance_transaction_notes (user_id, transaction_id, created_at, note_id);

CREATE TABLE finance_balance_snapshots (
  user_id TEXT NOT NULL,
  account_id TEXT NOT NULL,
  snapshot_id TEXT NOT NULL,
  balance BIGINT NOT NULL,
  occurred_at TIMESTAMPTZ NOT NULL,
  recorded_at TIMESTAMPTZ NOT NULL,
  reconciliation_status TEXT NOT NULL DEFAULT 'unreconciled' CHECK (reconciliation_status IN ('unreconciled', 'reconciled')),
  PRIMARY KEY (user_id, account_id, snapshot_id),
  UNIQUE (user_id, account_id, occurred_at)
);

CREATE INDEX finance_balance_snapshots_user_account_occurred_idx
  ON finance_balance_snapshots (user_id, account_id, occurred_at DESC, snapshot_id ASC);

CREATE INDEX finance_balance_snapshots_user_account_status_occurred_idx
  ON finance_balance_snapshots (user_id, account_id, reconciliation_status, occurred_at DESC, snapshot_id ASC);

CREATE TABLE finance_balance_snapshot_adjustments (
  user_id TEXT NOT NULL,
  account_id TEXT NOT NULL,
  snapshot_id TEXT NOT NULL,
  snapshot_occurred_at TIMESTAMPTZ NOT NULL,
  amount BIGINT NOT NULL CHECK (amount > 0),
  direction TEXT NOT NULL CHECK (direction IN ('sent', 'received')),
  reason TEXT NULL,
  recorded_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (user_id, account_id, snapshot_id),
  UNIQUE (user_id, account_id, snapshot_occurred_at, snapshot_id)
);

CREATE INDEX finance_balance_snapshot_adjustments_user_account_occurred_idx
  ON finance_balance_snapshot_adjustments (user_id, account_id, snapshot_occurred_at DESC, snapshot_id ASC);
