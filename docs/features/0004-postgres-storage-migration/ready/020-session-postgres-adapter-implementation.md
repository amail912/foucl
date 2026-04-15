# 020 Session Postgres Adapter Implementation

## Objective

Implement Postgres-backed session repository behavior against the `SessionRepository` contract using the shared generic `Repository` error model.

## Scope

- Implement Postgres adapter for all session repository operations defined by story 017.
- Use schema and migrations delivered by story 022.
- Integrate adapter into session backend selection path delivered by story 018.
- Use `postgresql-simple` directly for repository queries and commands.
- Emit only shared generic repository errors from the common `Repository` module.
- Align adapter behavior with existing session/auth business mapping semantics.
- Consume top-level split `database` config (`host`, `port`, `name`, `user`, `password`) through startup composition.

## Operational Contract

- Repository operations implemented: create/delete handle, create/load/update/delete state, create/delete user-state binding, delete all user-state bindings for a user.
- Adapter must rely on 022 schema guarantees for keys, constraints, and lookup/index support required by session flows.
- Adapter must preserve session semantics required by business layer for state validity handling, idle refresh/touch support, and revoke flows.
- Adapter must not introduce backend-specific error classes.

## Error Mapping Contract

- Duplicate record conflict -> `AlreadyExists`
- Missing targeted record -> `NotFound`
- Query/read/decode failures -> `ReadFailure`
- Write/update/delete failures -> `WriteFailure`
- Connection/storage-layer failures -> `StorageFailure`

## Dependencies And Boundaries

- Depends on:
  - 017 Session Repository Contract
  - 018 Session Backend Selection And Wiring
  - 022 Session Postgres Schema And Migrations

Out of scope:

- Session Postgres parity verification signoff (021).
- Session filesystem parity implementation (019).
- Startup import behavior (023/024).

## Acceptance Criteria

1. Postgres adapter satisfies full `SessionRepository` operation contract.
2. Adapter emits only shared repository errors (`AlreadyExists`, `NotFound`, `ReadFailure`, `WriteFailure`, `StorageFailure`).
3. Adapter behavior preserves existing session/auth business mapping semantics without introducing new client-visible error classes.
4. Adapter assumes DB-level schema guarantees from 022 and does not define alternative runtime schema fallback policy.
5. With `sessionBackend=postgres`, session runtime composition uses Postgres adapter path only.
6. No HTTP route, payload, status code, cookie behavior, or message drift is introduced.

## Test Cases And Scenarios

- Unit tests for adapter success/failure paths across all repository operations.
- Tests verifying each failure class maps to the required shared repository error.
- Contract tests verifying adapter satisfies `SessionRepository` and does not leak backend-specific errors.
- Real-Postgres parity signoff is delegated to story 021 with strict no-mock verification mode.

## Out Of Scope

- Full parity verification signoff.
- Startup import behavior.
