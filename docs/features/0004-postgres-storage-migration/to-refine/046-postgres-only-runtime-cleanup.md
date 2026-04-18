# 046 Postgres-Only Runtime Cleanup

## Objective

Finalize migration closeout by removing dead cross-domain backend-selection scaffolding and aligning runtime/docs/tests to Postgres-only persistence.

## Scope

- Remove obsolete runtime branching and backend-selection code that exists only to support filesystem persistence modes.
- Consolidate startup and runtime wiring to Postgres-only persistence assumptions for migrated domains.
- Remove obsolete startup-import fixture dependencies that only served filesystem bootstrap coverage.
- Align docs and test expectations with Postgres-only runtime contract.

## Dependencies And Boundaries

Depends on:

- 043 Auth Session Filesystem Removal.
- 044 Calendar Trip-Sharing Filesystem Removal.
- 045 Notes Checklists Filesystem Removal.

Out of scope:

- New feature behavior.
- Persistence model redesign beyond removing legacy filesystem runtime support.

## Acceptance Criteria

1. No dead backend-selection branches remain for migrated domain persistence.
2. Runtime configuration and startup errors consistently communicate Postgres-only persistence expectations.
3. Documentation and tests are aligned with the Postgres-only runtime contract.
4. Full test suite passes after cleanup.
