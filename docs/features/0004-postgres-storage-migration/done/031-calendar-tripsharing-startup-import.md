# 031 Calendar And Trip Sharing Startup Import

## Objective

Add startup filesystem-to-Postgres bootstrap import for calendar and trip-sharing domains when those domains are switched to Postgres.

## Scope

- Run import only when `calendarBackend=postgres` and/or `tripSharingBackend=postgres`.
- Read filesystem domain data and existing Postgres domain data at startup.
- Use deterministic conflict policy (`postgres-wins`) and import ordering that respects schema relations.
- Emit one domain overlap warning when both sources contain data.
- Emit conflict warnings with domain/key context for skipped filesystem conflicts.
- Keep import idempotent and safe across repeated startup runs.

## Dependencies And Boundaries

Depends on:

- 026 Calendar And Trip Sharing Backend Selection And Wiring.
- 028 Calendar And Trip Sharing Postgres Schema And Migrations.
- 029 Calendar And Trip Sharing Postgres Adapter Implementation.

Out of scope:

- Auth/session startup import behavior.
- Notes/checklists startup import behavior.
- Non-startup migration tooling.

## Acceptance Criteria

1. Calendar/trip-sharing imports are gated by domain backend flags.
2. Overlap warnings and conflict warnings are emitted with domain/key context.
3. Filesystem-only records are imported deterministically.
4. Postgres-conflicting records preserve Postgres values.
5. Repeated startup runs preserve stable end state without incorrect duplicates.
6. No calendar/trip-sharing HTTP contract drift is introduced.
