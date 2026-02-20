# Foucl - A Happstack server used with the `favs` frontend

## Configuration

Required env vars:
- `FOUCL_SESSION_SECRET`: secret used to sign session cookies.

Optional env vars:
- `FOUCL_CONFIG_FILE`: path to the JSON config file (default: `config/app-config.json`).
- `FOUCL_SESSION_COOKIE_SECURE`: set to `false` (or `0`) to allow HTTP in dev. Default is `true`.

## API

Auth:
- `POST /api/signup`
- `POST /api/signin`
- `POST /api/signout`

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
- `POST /api/v1/calendar-items/:id/validate`

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
