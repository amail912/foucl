# 042 Runtime Docs Alignment For Startup Migrations

## Objective

Align migration and operational documentation with automatic startup migration behavior.

## Scope

- Update migration-system and feature docs to reflect runtime startup migration execution.
- Document startup execution order:
  1. migration,
  2. storage verification,
  3. startup import,
  4. HTTP serve.
- Clarify operational expectations across local/dev/test/prod for startup auto-migrate.
- Remove stale wording that implies schema must always be pre-applied externally.

## Dependencies And Boundaries

Depends on:

- 039 Startup Migration Orchestrator.
- 040 Startup Migration Observability And Failure Contract.

Out of scope:

- New feature behavior beyond documentation alignment.
- Policy changes after auto-migrate adoption.

## Acceptance Criteria

1. `migration-system.md` and migration backlog docs describe startup auto-migrate behavior consistently.
2. Documentation defines fail-fast behavior on migration failure.
3. Documentation reflects current fixture ownership model (test harness/resources for startup-import fixtures).
4. No conflicting guidance remains about startup migration responsibility.
