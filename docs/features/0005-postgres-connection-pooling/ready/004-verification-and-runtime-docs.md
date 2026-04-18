# 004 Verification And Runtime Docs

## Objective

Lock pooling-cutover behavior with regression verification and runtime documentation alignment.

## Scope

- Add/adjust integration coverage to preserve route/auth semantics around unknown vs protected API paths.
- Add/adjust session-focused regression checks validating touch-write behavior after story 003.
- Update runtime documentation to describe shared Postgres pool architecture.

## Implementation Decisions

- Required verification gate:
  - `make lint`
  - `CABAL_DIR=/tmp/cabal-home make test`
  - `CABAL_DIR=/tmp/cabal-home make integration-test-postgres`
- Unknown-path/auth expectations to preserve:
  - unknown unauthenticated `/api/...` path -> `404`
  - known protected unauthenticated path -> `401`
- Documentation updates must include:
  - session technical runtime flow with pooled connections,
  - any auth/admin API contract wording impacted by preserved status semantics.

## Dependencies And Boundaries

Depends on:

- 002 Repository Cutover To WithResource.
- 003 Session Hot-Path Write Reduction.

Out of scope:

- CI performance SLA enforcement.
- logging format/pipeline redesign.

## Acceptance Criteria

1. Full verification gate passes.
2. Runtime docs describe pooled Postgres access and no longer imply per-operation connect/close.
3. Route/auth status semantics remain contract-consistent and covered by tests.
4. No API contract payload changes are introduced.
