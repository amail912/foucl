CREATE TYPE auth_user_role AS ENUM ('admin', 'member');

CREATE TABLE auth_users (
  username TEXT PRIMARY KEY,
  password_hash TEXT NOT NULL,
  role auth_user_role NOT NULL,
  approved BOOLEAN NOT NULL
);
