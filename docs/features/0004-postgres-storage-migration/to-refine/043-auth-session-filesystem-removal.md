# 043 Auth Session Filesystem Removal

## Objective

Remove filesystem runtime persistence paths for auth and session domains, leaving Postgres-only runtime behavior.

## Scope

- Remove filesystem-backed auth and session repository/store composition paths.
- Remove auth/session startup filesystem-to-Postgres import routines and related filesystem source handling.
- Keep startup sequence coherent with Postgres-only runtime:
  1. startup migrations,
  2. Postgres storage wiring/verification,
  3. remaining startup import passes,
  4. HTTP serve.
- Enforce cutover policy for config values:
  - `auth.authBackend=filesystem` fails startup with actionable error.
  - `session.sessionBackend=filesystem` fails startup with actionable error.

## Dependencies And Boundaries

Depends on:

- 023 Auth Startup Import.
- 024 Session Startup Import.
- 039 Startup Migration Orchestrator.

Out of scope:

- Calendar/trip-sharing removal.
- Notes/checklists removal.
- Cross-domain runtime cleanup and final doc consolidation (046).

## Acceptance Criteria

1. No runtime auth/session code path reads or writes filesystem persistence.
2. Startup import no longer executes auth/session filesystem import logic.
3. Startup fails clearly when legacy filesystem backend config values are used for auth/session.
4. Auth/session API contract remains behavior-compatible under Postgres runtime.
5. Postgres integration and unit tests remain green after removal.
