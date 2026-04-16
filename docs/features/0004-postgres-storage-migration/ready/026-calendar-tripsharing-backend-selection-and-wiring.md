# 026 Calendar And Trip Sharing Backend Selection And Wiring

## Objective

Introduce runtime backend selection and startup wiring for calendar and trip-sharing domains, composing exactly one backend per domain.

## Scope

- Add domain flags:
  - `calendarBackend` (`filesystem|postgres`)
  - `tripSharingBackend` (`filesystem|postgres`)
- Validate flags at startup; invalid values fail startup.
- In `postgres` mode, require top-level `database` split config (`host`, `port`, `name`, `user`, `password`).
- Wire startup composition to selected repositories only.
- Emit startup logs for selected calendar/trip-sharing backends.

## Dependencies And Boundaries

Depends on:

- 025 Calendar And Trip Sharing Repository Contracts.

Out of scope:

- Postgres schema and adapter behavior.
- Domain parity signoff.
- Startup filesystem-to-Postgres import behavior.

## Acceptance Criteria

1. Calendar/trip-sharing backend selection is config-driven per domain.
2. Invalid backend values fail startup before serving traffic.
3. `postgres` mode fails startup when DB config is missing.
4. Exactly one backend implementation is composed per domain in a runtime.
5. Startup logs include selected backend per domain.
