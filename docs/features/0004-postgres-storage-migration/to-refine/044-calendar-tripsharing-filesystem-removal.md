# 044 Calendar Trip-Sharing Filesystem Removal

## Objective

Remove filesystem runtime persistence paths for calendar and trip-sharing domains, leaving Postgres-only runtime behavior.

## Scope

- Remove filesystem-backed calendar and trip-sharing repository composition paths.
- Remove calendar/trip-sharing startup filesystem-to-Postgres import routines and related filesystem source handling.
- Enforce cutover policy for config values:
  - `calendarBackend=filesystem` fails startup with actionable error.
  - `tripSharingBackend=filesystem` fails startup with actionable error.

## Dependencies And Boundaries

Depends on:

- 031 Calendar And Trip Sharing Startup Import.
- 039 Startup Migration Orchestrator.

Out of scope:

- Auth/session removal.
- Notes/checklists removal.
- Final cross-domain cleanup and doc consolidation (046).

## Acceptance Criteria

1. No runtime calendar/trip-sharing code path reads or writes filesystem persistence.
2. Startup import no longer executes calendar/trip-sharing filesystem import logic.
3. Startup fails clearly when legacy filesystem backend config values are used for calendar/trip-sharing.
4. Calendar/trip-sharing API contract remains behavior-compatible under Postgres runtime.
5. Postgres integration and unit tests remain green after removal.
