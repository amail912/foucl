# Postgres Connection Pooling

This backlog removes Postgres connection churn on request paths by introducing shared application-level connection pooling while keeping API behavior stable.

## Goal

Reduce request latency and DB connection overhead by replacing per-operation `connectPostgreSQL`/`close` patterns with pooled connection reuse.

## Scope For This Iteration

- Add runtime connection-pool infrastructure using `resource-pool` with `postgresql-simple`.
- Migrate Postgres repository call paths to pooled acquisition in one implementation pass.
- Reduce session hot-path write amplification where no idle-expiry change is required.
- Preserve route contracts, payload shapes, status semantics, and startup fail-fast behavior.

## Delivery Decisions

- Pooling stack: `Data.Pool` (`resource-pool`) + `postgresql-simple`.
- Rollout mode: one big cutover for runtime repositories (not incremental by domain).
- No logging-system redesign in this feature.
- Startup migration/import orchestration semantics remain unchanged.

## Stories

### Done

- [001 Pooling Contract And Runtime Wiring](done/001-pooling-contract-and-runtime-wiring.md)

### Ready

- [002 Repository Cutover To WithResource](ready/002-repository-cutover-to-withresource.md)
- [003 Session Hot-Path Write Reduction](ready/003-session-hotpath-write-reduction.md)
- [004 Verification And Runtime Docs](ready/004-verification-and-runtime-docs.md)
