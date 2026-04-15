# 004 Domain Switching And Bootstrap Import

## Objective

Enable per-domain backend switching and startup filesystem-to-Postgres bootstrap import for switched domains.

## Scope

- Add one global Postgres connection configuration.
- Add per-domain backend flags with values `filesystem` or `postgres`.
- Add startup import routines for Postgres-enabled domains.
- When both filesystem and Postgres contain domain data, log a warning and still upsert filesystem data into Postgres.
- Ensure import routines are idempotent and safe to run on every startup.

## Acceptance Criteria

1. Server supports mixed backend modes across domains in one run.
2. Import runs only for domains configured with `postgres` backend.
3. Warning log is emitted when both stores already contain data for a switched domain.
4. Startup import upserts filesystem data into Postgres deterministically.
5. Repeated startup imports do not produce incorrect duplicates or drift.

## Out Of Scope

- Domain-specific repository migrations (covered by separate stories).
