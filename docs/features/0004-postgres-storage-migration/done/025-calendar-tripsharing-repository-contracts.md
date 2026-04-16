# 025 Calendar And Trip Sharing Repository Contracts

## Objective

Define persistence-agnostic repository contracts for calendar items and trip-sharing relations so business/controller behavior stays stable across filesystem and Postgres backends.

## Scope

- Introduce repository interfaces for:
  - calendar item create/load/list/update/delete,
  - calendar duration update used by validate flows,
  - trip-sharing share/subscription list/add/delete.
- Preserve period-trips read semantics needed by existing API behavior.
- Use shared generic repository error categories; do not expose backend-specific types.
- Keep runtime domain identifiers and business types stable.

## Contract Requirements

- No filesystem paths or SQL-specific types in the interface.
- Deterministic ordering requirements are explicit for list operations.
- Error mapping expectations are explicit and shared across both domains.

## Dependencies And Boundaries

Depends on:

- 024 Session Startup Import (pattern reference for migration sequencing only).

Out of scope:

- Backend selection and startup composition wiring.
- Postgres schema/migrations.
- Adapter implementation and parity signoff.
- Startup import execution.

## Acceptance Criteria

1. Calendar/trip-sharing repository contracts are persistence-agnostic.
2. Contract-required list ordering and visibility semantics are explicit.
3. Shared repository error model is used and backend-specific errors are not exposed.
4. Existing business/controller call sites can depend on contracts without HTTP behavior drift.

## Implementation Decisions

- New modules `CalendarRepository` and `TripSharingRepository` define persistence-agnostic repository records for both domains.
- Filesystem-backed default repository implementations were added in these new modules by adapting existing filesystem storage modules.
- `Lib` calendar/trip-sharing call sites were migrated to repository usage, removing direct filesystem storage imports from controller/business call paths.
- Repository methods emit shared `RepositoryError` values (`NotFound`, `ReadFailure`, `WriteFailure`) and do not leak storage-specific error types.
- Deterministic list behavior is explicit at repository level:
  - calendar items are listed in stable `itemId` order,
  - trip-sharing user lists remain stable/sorted.
