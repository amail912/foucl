# 022 Session Postgres Schema And Migrations

## Objective

Define and deliver Postgres schema and migration artifacts required for session persistence while preserving current session behavior.

## Scope

- Define Postgres schema for persisted session handles, session states, and user-state bindings required by the `SessionRepository` contract.
- Ensure schema is consumable by runtime adapters implemented with `postgresql-simple`.
- Use native Postgres `uuid` columns for `session_id` and `state_id`.
- Enforce one active user-state binding per user.
- Define FK relations with restrict/no-cascade delete behavior.
- Define constraints and indexes required for token/state lookup, handle/state linking, and revoke/revoke-all flows.
- Define deterministic up and down migrations for creating and evolving session schema.
- Ensure migration artifacts are executable in target environments.
- Ensure startup schema sanity checks can validate required session tables/constraints before serving requests in postgres mode.
- Ensure lookup support exists for repository load operations on handle/state/binding identifiers.

## Schema Contract

- `session_states` table:
  - `state_id uuid` primary key
  - `user_id text` not null
  - `created_at timestamptz` not null
  - `expires_at timestamptz` not null
  - `idle_expires_at timestamptz` not null
  - `revoked_at timestamptz` nullable
- `session_handles` table:
  - `session_id uuid` primary key
  - `state_id uuid` not null references `session_states(state_id)` on delete restrict
  - `issued_at timestamptz` not null
  - `revoked_at timestamptz` nullable
- `session_user_bindings` table:
  - `user_id text` primary key (one active binding per user)
  - `state_id uuid` not null references `session_states(state_id)` on delete restrict

Required indexes:

- index on `session_handles(state_id)` for revoke-all-by-state lookup.
- index on `session_user_bindings(state_id)` for state-to-user binding traversal and maintenance operations.
- index on `session_states(user_id)` for user-scoped state lookup paths.

## Migration Contract

- Up migrations create all required session schema objects (tables, constraints, indexes).
- Down migrations reverse created session schema objects in safe dependency order.
- Up/down/up execution is deterministic and repeatable.

## Acceptance Criteria

1. Up migrations run successfully on a clean database.
2. Down migrations run successfully from latest migrated state.
3. Re-applying up migrations after down succeeds.
4. `session_id` and `state_id` are persisted as Postgres `uuid` columns.
5. Schema supports all persisted fields and relations required by story 017 operations.
6. One active user-state binding per user is enforced by schema constraints.
7. FK delete behavior is restrict/no-cascade for session-state relations.
8. Constraints/indexes support expected token/state/user lookup and revoke flows.
9. No HTTP route, payload, status code, cookie behavior, or message drift is introduced.

## Out Of Scope

- Session Postgres adapter implementation details.
- Session parity verification execution.
- Startup import behavior.
