# 029 Calendar And Trip Sharing Postgres Adapter Implementation

## Objective

Implement Postgres-backed adapters for calendar and trip-sharing repositories while preserving contract behavior.

## Scope

- Implement adapters with `postgresql-simple`.
- Map storage/query failures to the shared repository error model.
- Preserve deterministic ordering and visibility semantics required by period-trips and list endpoints.
- Integrate adapters into backend wiring path from 026.

## Dependencies And Boundaries

Depends on:

- 025 Calendar And Trip Sharing Repository Contracts.
- 026 Calendar And Trip Sharing Backend Selection And Wiring.
- 028 Calendar And Trip Sharing Postgres Schema And Migrations.

Out of scope:

- Real Postgres parity signoff.
- Startup import behavior.

## Acceptance Criteria

1. Postgres adapters satisfy full contract operations for both domains.
2. Adapters use `postgresql-simple` only (no beam dependency introduced).
3. Adapter errors map only to shared repository error categories.
4. In postgres backend mode, runtime uses Postgres adapters only for switched domains.
