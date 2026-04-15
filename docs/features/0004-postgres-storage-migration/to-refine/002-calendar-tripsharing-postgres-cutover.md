# 002 Calendar And Trip Sharing Postgres Cutover

## Objective

Migrate calendar item storage and trip-sharing storage to Postgres while preserving current API behavior.

## Scope

- Introduce storage abstractions for calendar and trip-sharing persistence.
- Implement Postgres repositories using beam.
- Preserve validation and selection behavior for:
  - trip write validations
  - period-trips visibility and ordering semantics
- Route calendar and trip-sharing domains via per-domain backend flags.
- Ensure Postgres is the only runtime backend for these domains when switched.

## Acceptance Criteria

1. Calendar item create/list/update/validate/delete behavior remains contract-compatible.
2. Trip-sharing shares/subscriptions behavior remains contract-compatible.
3. Period-trips behavior remains contract-compatible, including ordering and visibility rules.
4. Existing integration tests for calendar and trip-sharing pass in Postgres mode.
5. Filesystem calendar and trip-sharing implementations are not used when domains are set to `postgres`.

## Out Of Scope

- Auth/session migration.
- Notes/checklists migration.
