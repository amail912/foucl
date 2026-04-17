# 041 Startup Migration Integration Verification

## Objective

Verify automatic startup migration behavior in real Postgres integration flows.

## Scope

- Extend integration-postgres coverage for startup migration scenarios:
  - clean database bootstraps schema on startup,
  - pre-migrated database startup is stable no-op,
  - migration failure prevents startup serving.
- Keep startup-import and parity suites passing with startup auto-migration enabled.
- Keep fixture ownership in test harness (`test/resources` + test setup code), not `Makefile` data generation.

## Dependencies And Boundaries

Depends on:

- 039 Startup Migration Orchestrator.
- 040 Startup Migration Observability And Failure Contract.

Out of scope:

- Unit-level SQL migration definition testing already covered by migration stories.

## Acceptance Criteria

1. Real Postgres integration tests pass for clean-DB and pre-migrated startup paths.
2. A failing migration path is verified to abort startup before HTTP is served.
3. Existing integration-postgres startup-import/parity scenarios remain green.
4. Test setup does not rely on `Makefile`-embedded fixture payloads.
