DROP INDEX IF EXISTS idx_session_states_user_id;
DROP INDEX IF EXISTS idx_session_user_bindings_state_id;
DROP INDEX IF EXISTS idx_session_handles_state_id;

DROP TABLE IF EXISTS session_user_bindings;
DROP TABLE IF EXISTS session_handles;
DROP TABLE IF EXISTS session_states;
