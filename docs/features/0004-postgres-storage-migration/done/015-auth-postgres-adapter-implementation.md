# 015 Auth Postgres Adapter Implementation

## Objective

Implement Postgres-backed auth repository behavior against the `AuthRepository` contract using the shared generic `Repository` error model.

## Scope

- Implement Postgres adapter for all auth repository operations defined by story 011.
- Use schema and migrations delivered by story 014.
- Use `postgresql-simple` directly for repository queries and commands.
- Emit only shared generic repository errors from the common `Repository` module.
- Align adapter behavior with existing auth business mapping semantics.
- Treat `role` and `approved` as DB-governed non-null constrained fields (no adapter fallback logic for missing values).
- Consume top-level `database` split fields (`host`, `port`, `name`, `user`, `password`) and compose a startup connection string.
- In `authBackend=postgres` mode, fail startup when DB connection or auth schema sanity checks fail.

## Operational Contract

- Repository operations implemented: create, load by username, update, delete by username, list.
- Adapter must rely on 014 schema guarantees for role and approval integrity.
- Adapter must not introduce backend-specific error classes.

## Error Mapping Contract

- Duplicate username conflict -> `AlreadyExists`
- Missing row in targeted operations -> `NotFound`
- Query/read/decode failures -> `ReadFailure`
- Write/update/delete failures -> `WriteFailure`
- Connection/storage-layer failures -> `StorageFailure`

## Acceptance Criteria

1. Postgres adapter satisfies full `AuthRepository` operation contract.
2. Adapter emits only shared repository errors (`AlreadyExists`, `NotFound`, `ReadFailure`, `WriteFailure`, `StorageFailure`).
3. Adapter behavior supports existing auth business mapping semantics without introducing new client-visible error classes.
4. Adapter assumes DB-level defaults and non-null constraints for role/approved, with no runtime fallback policy.
5. Runtime wiring uses top-level split `database` config fields and no per-domain DB credentials.
6. Startup fails fast in postgres auth mode when DB connect or auth schema validation fails.
7. No HTTP contract drift is introduced.

## Test Cases And Scenarios

- Unit tests covering operation success and failure paths for create/load/update/delete/list.
- Tests verifying each failure class maps to the required shared repository error.
- Contract tests verifying adapter satisfies `AuthRepository` and does not leak backend-specific errors.

## Out Of Scope

- Schema migration definition.
- Runtime backend wiring behavior.
- Startup import behavior.
- Full parity verification signoff.
