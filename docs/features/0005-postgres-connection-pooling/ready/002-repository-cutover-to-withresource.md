# 002 Repository Cutover To WithResource

## Objective

Replace runtime per-operation Postgres connect/close calls with pooled connection acquisition in one pass.

## Scope

- Migrate runtime Postgres access in:
  - auth repository
  - session repository/store Postgres adapter
  - calendar repository
  - trip-sharing repository
  - notes/checklists repository
- Replace repository-local `withPgConnection` implementations from:
  - `connectPostgreSQL` + action + `close`
  to:
  - `withResource pool` + action.
- Remove explicit per-operation connection close in runtime CRUD/read/write paths.

## Implementation Decisions

- Keep repository operation signatures and domain error mappings behavior-compatible.
- Keep SQL texts and query semantics unchanged in this story.
- Keep startup migration/import one-off direct-connection code unchanged in this story.
- Ensure all Postgres repository constructors consume shared pool dependency.

## Dependencies And Boundaries

Depends on:

- 001 Pooling Contract And Runtime Wiring.

Out of scope:

- query-shape optimization or schema/index changes.
- session touch-write reduction (story 003).
- logging redesign.

## Acceptance Criteria

1. Runtime repository operations no longer call `connectPostgreSQL`/`close` per request operation.
2. All repository calls use pooled `withResource` acquisition.
3. Existing API contracts remain unchanged (`status`, messages, payload shapes).
4. Unit tests and Postgres integration tests pass after cutover.
