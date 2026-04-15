-- Initialization script for local auth postgres parity tests.
-- Executed once when the data directory is first created.

ALTER DATABASE foucl SET timezone TO 'UTC';
ALTER ROLE foucl SET timezone TO 'UTC';

GRANT ALL PRIVILEGES ON DATABASE foucl TO foucl;
GRANT ALL ON SCHEMA public TO foucl;

-- Optional extension commonly useful in tests.
CREATE EXTENSION IF NOT EXISTS pgcrypto;
