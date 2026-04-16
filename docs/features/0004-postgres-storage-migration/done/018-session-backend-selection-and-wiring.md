# 018 Session Backend Selection And Wiring

## Objective

Introduce runtime backend selection for the session domain and wire startup composition through exactly one selected backend implementation.

## Scope

- Add session domain backend flag `sessionBackend` with supported values `filesystem` or `postgres`.
- Define default behavior: when `sessionBackend` is omitted, use `filesystem`.
- Validate backend flag at startup; unsupported values fail startup configuration.
- Wire startup composition so session logic receives exactly one `SessionRepository` implementation selected from `sessionBackend`.
- Emit startup log for selected session backend.
- Consume top-level split `database` config (`host`, `port`, `name`, `user`, `password`) for postgres session mode.

## Runtime Selection Contract

- `sessionBackend=filesystem` selects only filesystem session composition.
- `sessionBackend=postgres` selects only the Postgres session composition path (no fallback), and fails fast until the Postgres adapter is delivered.
- Mixed session backend composition in one runtime is not allowed.
- In postgres mode, startup must fail fast if database config is missing.

## Startup And Composition Contract

- Backend selection is resolved before serving requests.
- Invalid backend value fails startup configuration and prevents serving traffic.
- Route/controller layers keep using the stable `SessionStore` API and remain persistence-agnostic.
- Session runtime behavior remains stable across backends from client point of view.

## Dependencies And Boundaries

- Depends on: 017 Session Repository Contract.
- Unblocks:
  - 020 Session Postgres Adapter Implementation.
  - 021 Session Postgres Parity Verification.
  - 019 Session Filesystem Adapter Parity.

Out of scope:

- Session repository contract definition (017).
- Postgres adapter implementation/parity execution (020/021).
- Filesystem adapter implementation/parity execution (019).
- Startup import behavior (023/024).

## Acceptance Criteria

1. Session backend selection is driven by `sessionBackend` config.
2. Missing `sessionBackend` defaults to `filesystem`.
3. Invalid `sessionBackend` value fails startup before serving requests.
4. Filesystem mode composes session with filesystem implementation only.
5. Postgres mode does not fallback to filesystem and fails fast before serving traffic until Postgres adapter wiring is available.
6. Startup logs include selected session backend.
7. No HTTP route, payload, status code, cookie behavior, or message drift is introduced.

## Test Cases And Scenarios

- Configuration parsing tests:
  - omitted `sessionBackend` -> `filesystem` default,
  - `sessionBackend=filesystem` accepted,
  - `sessionBackend=postgres` accepted,
  - invalid value rejected with startup config error.
- Wiring selection tests:
  - filesystem mode composes session filesystem path only,
  - postgres mode rejects missing database config,
  - postgres mode fails fast with explicit non-fallback wiring error before adapter implementation,
  - no mixed composition path.
- Regression expectation reference:
  - session API behavior remains unchanged while wiring strategy changes.
