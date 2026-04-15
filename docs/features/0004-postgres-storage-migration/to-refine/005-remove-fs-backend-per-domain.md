# 005 Remove Filesystem Backend Per Migrated Domain

## Objective

Remove filesystem backend implementations as each domain migration completes.

## Scope

- Remove filesystem persistence modules for migrated domains.
- Remove dead backend routing branches for migrated domains.
- Keep docs and tests aligned to migrated backend expectations.

## Acceptance Criteria

1. Build and tests pass after each domain filesystem backend removal.
2. No runtime path remains that reads or writes filesystem storage for removed domains.
3. API behavior remains contract-compatible after removal.

## Out Of Scope

- Introducing new API behavior.
- Data model redesign beyond what migration requires.
