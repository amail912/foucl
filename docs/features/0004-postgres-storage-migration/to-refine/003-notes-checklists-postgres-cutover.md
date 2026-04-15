# 003 Notes And Checklists Postgres Cutover

## Objective

Migrate notes and checklists storage to Postgres while preserving current CRUD behavior and response contracts.

## Scope

- Add Postgres-backed CRUD implementation compatible with existing service usage.
- Preserve optimistic versioning semantics derived from content hash and `StorageId` behavior.
- Keep endpoint behavior and payload shapes stable.
- Route notes and checklists through per-domain backend flags.
- Ensure Postgres is the only runtime backend for these domains when switched.

## Acceptance Criteria

1. Existing notes and checklists CRUD tests pass in Postgres mode.
2. Error semantics for read/write/modify/delete remain compatible.
3. No filesystem code path is used for notes/checklists when domains are set to `postgres`.

## Out Of Scope

- Auth/session migration.
- Calendar/trip-sharing migration.
