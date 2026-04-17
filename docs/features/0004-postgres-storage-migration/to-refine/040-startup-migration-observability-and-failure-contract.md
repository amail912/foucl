# 040 Startup Migration Observability And Failure Contract

## Objective

Define deterministic startup logging and failure semantics for automatic migration execution.

## Scope

- Define startup log entries for:
  - migration orchestration start,
  - per-domain migration execution,
  - completion summary,
  - failure summary.
- Ensure logs include domain context and direction (`MigrateUp`).
- Ensure startup failure messages are actionable when migration fails.
- Preserve fail-fast startup contract (no HTTP serving after migration failure).

## Dependencies And Boundaries

Depends on:

- 039 Startup Migration Orchestrator.

Out of scope:

- External log aggregation integration.
- Retry/backoff runtime policies for failed migrations.

## Acceptance Criteria

1. Logs consistently show which Postgres domains were migrated or skipped.
2. Migration failures include domain context and are surfaced as startup errors.
3. Successful startup after migration emits completion logs before repository wiring/serve phase.
4. Existing startup logs for verification/import remain coherent with migration logs.
