# Foucl - A Happstack server used with the `favs` frontend

## Configuration

Required env vars:
- `FOUCL_SESSION_SECRET`: secret used to sign session cookies.

Optional env vars:
- `FOUCL_CONFIG_FILE`: path to the JSON config file (default: `config/app-config.json`).
- `FOUCL_SESSION_COOKIE_SECURE`: set to `false` (or `0`) to allow HTTP in dev. Default is `true`.

Required config fields:
- `auth.bootstrapAdminUsername`: username that is bootstrapped as the first approved admin.
- When using `auth.authBackend = "postgres"`, a top-level `database` object is required:
  - `database.host`
  - `database.port`
  - `database.name`
  - `database.user`
  - `database.password`

## API

Auth:
- `POST /api/signup`
- `POST /api/signin`
- `POST /api/signout`

Signup behavior:
- `POST /api/signup` creates a pending account for normal users.
- Pending users cannot sign in until an admin approves them.
- The configured bootstrap admin username is created as an approved admin account.

Admin (auth required, admin only):
- `GET /api/v1/admin/pending-signups`
- `POST /api/v1/admin/pending-signups/approve`

Notes (auth required):
- `GET /api/note`
- `POST /api/note`
- `PUT /api/note`
- `DELETE /api/note/:id`

Checklists (auth required):
- `GET /api/checklist`
- `POST /api/checklist`
- `PUT /api/checklist`
- `DELETE /api/checklist/:id`

Agenda (auth required):
- `GET /api/v1/calendar-items`
- `POST /api/v1/calendar-items`
- Update: `POST /api/v1/calendar-items` with `id` in body updates existing calendar item

## Lint

This project uses `hlint` as the Haskell linter.

Install:

```bash
cabal install hlint
```

Run:

```bash
make lint
```

## Integration tests

- Filesystem-backed integration suite:
  - `make integration-test`
- Postgres parity suite (auth + session, real DB, hard fail when unavailable):
  - `make integration-test-postgres`
  - Alias: `make integration-test-auth-postgres`
  - Uses fixed local test DB endpoint: `127.0.0.1:5432`, `dbname=foucl`, `user=foucl`, `password=foucl`.
  - The Make target orchestrates Docker automatically:
    - starts DB,
    - waits for readiness,
    - runs the parity suite,
    - stops DB and removes the volume.
  - Compose stack details:
    - Postgres image: `postgres:17`
    - Persistent volume: `auth-postgres-test-db-data`
    - Init SQL script (first startup only): `db/init/auth-postgres-tests-init.sql`
  - Reset DB volume when you need to re-run init SQL from scratch:
    - `docker compose -f docker-compose.auth-postgres-tests.yml down -v`
