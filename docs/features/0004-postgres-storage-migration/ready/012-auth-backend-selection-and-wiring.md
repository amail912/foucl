# 012 Auth Backend Selection And Wiring

## Objective

Introduce runtime backend selection for the auth domain and wire startup composition through exactly one selected backend implementation.

## Scope

- Add auth domain backend flag `authBackend` with supported values `filesystem` or `postgres`.
- Define default behavior: when `authBackend` is omitted, use `filesystem`.
- Validate backend flag at startup; unsupported values fail startup configuration.
- Wire startup composition so auth receives exactly one repository implementation selected from `authBackend`.
- Ensure route/controller layers consume auth business service dependencies and do not depend on persistence implementation details.
- Emit startup log for selected auth backend.

## Runtime Selection Contract

- `authBackend=filesystem` selects the filesystem auth repository composition path.
- `authBackend=postgres` selects the Postgres auth repository composition path.
- Mixed auth backend composition in one runtime is not allowed.
- This story consumes existing global Postgres connection configuration and does not define new database schema behavior.

## Dependencies And Boundaries

- Depends on: 011 Auth Repository Contract.
- Unblocks:
  - 015 Auth Postgres Adapter Implementation.
  - 013 Auth Filesystem Adapter Parity.

Out of scope:

- Auth repository contract design (011).
- Postgres repository implementation details (015).
- Filesystem adapter implementation details (013).
- Startup import behavior (023/024).
- Session backend selection and wiring (018), and session Postgres implementation/parity stories (020/021).

## Acceptance Criteria

1. Auth backend selection is driven by `authBackend` config.
2. Missing `authBackend` defaults to `filesystem`.
3. Invalid `authBackend` value fails startup before serving requests.
4. Filesystem mode selects only filesystem auth composition path.
5. Postgres mode selects only Postgres auth composition path.
6. Startup logs include selected auth backend.
7. No HTTP route, payload, status code, or message drift is introduced.

## Test Cases And Scenarios

- Configuration parsing tests:
  - omitted `authBackend` -> `filesystem` default
  - `authBackend=filesystem` accepted
  - `authBackend=postgres` accepted
  - invalid value rejected with startup config error
- Wiring selection tests:
  - filesystem mode composes auth with filesystem repository only
  - postgres mode composes auth with Postgres repository only
  - no mixed auth composition path
- Regression expectation reference:
  - auth API behavior remains unchanged while switching wiring strategy.
