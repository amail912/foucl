# 045 Notes Checklists Filesystem Removal

## Objective

Remove filesystem runtime persistence paths for notes and checklists domains, leaving Postgres-only runtime behavior.

## Scope

- Remove filesystem-backed notes/checklists repository composition paths.
- Remove notes/checklists startup filesystem-to-Postgres import routines and related filesystem source handling.
- Enforce cutover policy for config values:
  - `noteBackend=filesystem` fails startup with actionable error.
  - `checklistBackend=filesystem` fails startup with actionable error.

## Dependencies And Boundaries

Depends on:

- 038 Notes And Checklists Startup Import.
- 039 Startup Migration Orchestrator.

Out of scope:

- Auth/session removal.
- Calendar/trip-sharing removal.
- Final cross-domain cleanup and doc consolidation (046).

## Acceptance Criteria

1. No runtime notes/checklists code path reads or writes filesystem persistence.
2. Startup import no longer executes notes/checklists filesystem import logic.
3. Startup fails clearly when legacy filesystem backend config values are used for notes/checklists.
4. Notes/checklists API contract remains behavior-compatible under Postgres runtime.
5. Postgres integration and unit tests remain green after removal.
