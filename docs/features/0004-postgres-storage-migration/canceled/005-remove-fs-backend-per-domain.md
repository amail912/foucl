# 005 Remove Filesystem Backend Per Migrated Domain

## Status

Canceled as superseded.

## Rationale

This story is too broad for safe delivery because it combines:

- multi-domain filesystem persistence removal,
- runtime backend routing removal,
- startup import removal,
- post-cutover docs/test cleanup.

The work is split into executable closeout stories with domain-bounded ownership and explicit cutover policy.

## Replaced By

- 043 Auth Session Filesystem Removal.
- 044 Calendar Trip-Sharing Filesystem Removal.
- 045 Notes Checklists Filesystem Removal.
- 046 Postgres-Only Runtime Cleanup.

## Decision Notes

- Cutover policy is fail-fast for legacy filesystem backend config values.
- No remaining implementation work is owned by story 005.
