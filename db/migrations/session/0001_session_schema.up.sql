CREATE TABLE session_states (
  state_id uuid PRIMARY KEY,
  user_id text NOT NULL,
  created_at timestamptz NOT NULL,
  expires_at timestamptz NOT NULL,
  idle_expires_at timestamptz NOT NULL,
  revoked_at timestamptz
);

CREATE TABLE session_handles (
  session_id uuid PRIMARY KEY,
  state_id uuid NOT NULL REFERENCES session_states(state_id) ON DELETE RESTRICT,
  issued_at timestamptz NOT NULL,
  revoked_at timestamptz
);

CREATE TABLE session_user_bindings (
  user_id text PRIMARY KEY,
  state_id uuid NOT NULL REFERENCES session_states(state_id) ON DELETE RESTRICT
);

CREATE INDEX idx_session_handles_state_id ON session_handles(state_id);
CREATE INDEX idx_session_user_bindings_state_id ON session_user_bindings(state_id);
CREATE INDEX idx_session_states_user_id ON session_states(user_id);
