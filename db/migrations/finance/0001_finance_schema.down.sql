DROP INDEX IF EXISTS finance_balance_snapshots_user_account_status_occurred_idx;
DROP INDEX IF EXISTS finance_balance_snapshots_user_account_occurred_idx;
DROP INDEX IF EXISTS finance_balance_snapshot_adjustments_user_account_occurred_idx;
DROP TABLE IF EXISTS finance_balance_snapshot_adjustments;
DROP TABLE IF EXISTS finance_balance_snapshots;

DROP INDEX IF EXISTS finance_transaction_notes_user_transaction_created_idx;
DROP TABLE IF EXISTS finance_transaction_notes;

DROP INDEX IF EXISTS finance_transaction_links_user_peer_idx;
DROP TABLE IF EXISTS finance_transaction_links;

DROP INDEX IF EXISTS finance_transaction_splits_user_transaction_idx;
DROP TABLE IF EXISTS finance_transaction_splits;
DROP TABLE IF EXISTS finance_transaction_categories;

DROP INDEX IF EXISTS finance_categories_parent_idx;
DROP INDEX IF EXISTS finance_categories_user_parent_name_idx;
DROP TABLE IF EXISTS finance_categories;

DROP INDEX IF EXISTS finance_transactions_user_account_occurred_idx;
DROP INDEX IF EXISTS finance_transactions_user_occurred_idx;
DROP TABLE IF EXISTS finance_transaction_idempotency;
DROP TABLE IF EXISTS finance_transactions;

DROP INDEX IF EXISTS finance_accounts_user_status_name_idx;
DROP TABLE IF EXISTS finance_accounts;

DROP INDEX IF EXISTS finance_events_user_event_number_idx;
DROP TABLE IF EXISTS finance_events;
