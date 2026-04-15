# Postgres Storage Migration

This backlog defines a domain-by-domain migration from filesystem persistence to Postgres while keeping the API contract stable from consumer point of view.

## Goal

Move persistence to Postgres incrementally, with per-domain cutover control, and no observable API contract change for clients.

## Principles

- Keep HTTP routes, payload shapes, status codes, and error messages compatible.
- Use one active backend per domain at runtime.
- When a domain is switched to Postgres, it uses Postgres only.
- On startup for switched domains, import filesystem data into Postgres.
- If both filesystem and Postgres already contain data for a switched domain, log a warning; use deterministic conflict handling (domain policy, currently `postgres-wins`) and upsert non-conflicting filesystem data into Postgres.
- Remove filesystem implementation per migrated domain after cutover is complete.

## Migration Sequence

1. Auth and sessions.
2. Calendar items and trip sharing.
3. Notes and checklists.
4. Remove remaining filesystem backend code and dead wiring.

## Delivery Notes

- Backend activation is controlled per domain through configuration.
- Postgres connection is configured globally.
- Import routines must be idempotent and safe to run at each startup.

## Stories

### Done

- [011 Auth Repository Contract](done/011-auth-repository-contract.md)

### Ready

- [012 Auth Backend Selection And Wiring](ready/012-auth-backend-selection-and-wiring.md)
- [013 Auth Filesystem Adapter Parity](ready/013-auth-filesystem-adapter-parity.md)
- [014 Auth Postgres Schema And Migrations](ready/014-auth-postgres-schema-and-migrations.md)
- [015 Auth Postgres Adapter Implementation](ready/015-auth-postgres-adapter-implementation.md)
- [016 Auth Postgres Parity Verification](ready/016-auth-postgres-parity-verification.md)
- [017 Session Repository Contract](ready/017-session-repository-contract.md)
- [018 Session Backend Selection And Wiring](ready/018-session-backend-selection-and-wiring.md)
- [019 Session Filesystem Adapter Parity](ready/019-session-filesystem-adapter-parity.md)
- [020 Session Postgres Adapter Implementation](ready/020-session-postgres-adapter-implementation.md)
- [021 Session Postgres Parity Verification](ready/021-session-postgres-parity-verification.md)
- [022 Session Postgres Schema And Migrations](ready/022-session-postgres-schema-and-migrations.md)
- [023 Auth Startup Import](ready/023-auth-startup-import.md)
- [024 Session Startup Import](ready/024-session-startup-import.md)

### To Refine

- [002 Calendar And Trip Sharing Postgres Cutover](to-refine/002-calendar-tripsharing-postgres-cutover.md)
- [003 Notes And Checklists Postgres Cutover](to-refine/003-notes-checklists-postgres-cutover.md)
- [004 Domain Switching And Bootstrap Import](to-refine/004-domain-switching-and-bootstrap-import.md)
- [005 Remove Filesystem Backend Per Migrated Domain](to-refine/005-remove-fs-backend-per-domain.md)

### Canceled

- [001 Auth And Sessions Postgres Cutover](canceled/001-auth-session-postgres-cutover.md) - Canceled because it coupled two critical domains into one oversized story; replaced by 006, 007, 008, 022, 020, 021, 023, and 024.
- [006 Auth Repository And Wiring](canceled/006-auth-repository-and-wiring.md) - Canceled because it mixed contract definition, backend wiring, and filesystem parity work; replaced by 011, 012, and 013.
- [007 Auth Postgres Implementation And Parity](canceled/007-auth-postgres-implementation-and-parity.md) - Canceled because it mixed schema/migrations, adapter implementation, and parity verification; replaced by 014, 015, and 016.
- [008 Session Repository And Wiring](canceled/008-session-repository-and-wiring.md) - Canceled because it mixed session contract, backend wiring, and filesystem parity expectations; replaced by 017, 018, and 019.
- [009 Session Postgres Implementation And Parity](canceled/009-session-postgres-implementation-and-parity.md) - Canceled because it mixed Postgres adapter implementation and parity verification; replaced by 022, 020, and 021.
- [010 Auth Session Startup Import](canceled/010-auth-session-startup-import.md) - Canceled because it mixed auth and session startup import execution/verification; replaced by 023 and 024.
