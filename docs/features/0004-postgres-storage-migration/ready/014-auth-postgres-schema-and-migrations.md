# 014 Auth Postgres Schema And Migrations

## Objective

Define and deliver Postgres schema and migration artifacts required for auth persistence while preserving current auth behavior.

## Scope

- Define Postgres auth table schema required for persisted user data currently used by auth flows.
- Enforce case-sensitive username uniqueness and lookup behavior.
- Store role as a Postgres enum.
- Store approval state as a boolean column.
- Define deterministic up and down migrations for creating and evolving auth schema.
- Ensure schema constraints and indexes support current lookup and uniqueness behavior.

## Schema Contract

- `username`: unique, case-sensitive key.
- `password_hash`: required field for signin password verification flow.
- `role`: Postgres enum constrained to supported auth roles.
- `approved`: boolean field representing approval state.

## Migration Contract

- Up migrations must create enum type(s), table(s), constraints, and indexes required by auth persistence.
- Down migrations must reverse created schema objects in safe dependency order.
- Migrations must be deterministic and executable in target environments.

## Acceptance Criteria

1. Up migrations run successfully on a clean database.
2. Down migrations run successfully from latest migrated state.
3. Re-applying up migrations after down succeeds.
4. Username uniqueness is enforced with case-sensitive semantics.
5. Role values are restricted by Postgres enum definition.
6. Approval state is persisted as boolean with expected nullability/default behavior.
7. Schema supports all persisted fields required by current auth flows.
8. No HTTP contract drift is introduced.

## Out Of Scope

- Postgres repository adapter implementation.
- Runtime backend wiring behavior.
- Startup import behavior.
- Parity verification execution.
