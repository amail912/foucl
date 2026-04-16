CREATE TABLE trip_shares (
  owner_user_id text NOT NULL,
  target_username text NOT NULL,
  PRIMARY KEY (owner_user_id, target_username)
);

CREATE TABLE trip_subscriptions (
  owner_user_id text NOT NULL,
  target_username text NOT NULL,
  PRIMARY KEY (owner_user_id, target_username)
);

CREATE INDEX idx_trip_shares_owner ON trip_shares(owner_user_id);
CREATE INDEX idx_trip_shares_target ON trip_shares(target_username);
CREATE INDEX idx_trip_subscriptions_owner ON trip_subscriptions(owner_user_id);
CREATE INDEX idx_trip_subscriptions_target ON trip_subscriptions(target_username);
