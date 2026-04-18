# 001 Pooling Contract And Runtime Wiring

## Objective

Define and wire a shared Postgres connection pool used by all Postgres runtime repositories.

## Scope

- Add `resource-pool` dependency to the library build.
- Introduce a process-local `Pool Connection` created once during runtime startup.
- Create pool only when at least one runtime backend uses Postgres.
- Pass the pool through runtime wiring to Postgres repository constructors.
- Keep startup migration/import execution order unchanged.

## Implementation Decisions

- Pool library and DB client:
  - `Data.Pool` from `resource-pool`.
  - existing `postgresql-simple` `Connection`.
- Pool parameters for v1 (fixed in code):
  - `stripes = 1`
  - `idleTime = 60` seconds
  - `maxResourcesPerStripe = 16`
- Ownership/lifecycle:
  - pool is created in `runApp` runtime wiring.
  - pool is shared by all Postgres runtime repositories in the process.
  - no per-request or per-repository pool creation.
- Configuration surface:
  - no new JSON/env knobs in this story.

## Dependencies And Boundaries

Depends on:

- existing Postgres runtime wiring for auth/session/calendar/trip-sharing/note/checklist backends.

Out of scope:

- repository query-path migration to pooled access (story 002).
- session write-frequency optimization (story 003).

## Acceptance Criteria

1. Build compiles with `resource-pool` and updated constructor signatures.
2. Runtime creates at most one shared Postgres pool when Postgres backends are selected.
3. No HTTP contract behavior changes are introduced.
4. Startup fail-fast behavior on migration/storage errors remains unchanged.
