# 016 Auth Postgres Parity Verification

## Objective

Validate that auth behavior remains contract-compatible when `authBackend=postgres` with real Postgres-backed execution.

## Scope

- Execute auth-focused parity verification against real Postgres-backed repository behavior.
- Run parity as automated integration tests against a running app in `authBackend=postgres` mode.
- Use fixed local Postgres test endpoint for this story: `host=127.0.0.1`, `port=5432`, `dbname=foucl`, `user=foucl`, `password=foucl`.
- Verify client-visible behavior remains aligned with existing expectations at auth business/controller boundaries.
- Verify admin governance and approval behaviors remain compatible.
- Verify status/message semantics remain unchanged for covered scenarios.

## Verification Mode

- This story certifies real Postgres runtime integration for auth flows using the 015 adapter and 014 schema.
- Mock-only checks are insufficient for completion.
- Missing/unreachable Postgres test endpoint is a hard failure for this parity run (no skip fallback).

## Scenario Matrix

- Signup: success and conflict.
- Signin: success, invalid credentials, pending approval.
- Auth profile load: success and technical failure path.
- Admin pending and approved list operations: success and technical failure paths.
- Admin approve/delete operations: success, not found, conflict (where applicable).

## Acceptance Criteria

1. Real Postgres-backed parity checks pass for all scenarios in scope.
2. Signup, signin, profile, pending/approved admin operations, and deletion semantics remain behavior-compatible.
3. Status codes and error messages remain compatible with current contract at the business/controller boundary.
4. No new client-visible error category is introduced.
5. Test setup deterministically resets auth schema/data before scenarios execute.
6. Story explicitly documents that real Postgres integration verification is in scope and required.
7. A dedicated executable test path exists for this parity run (`make integration-test-postgres` or alias `make integration-test-auth-postgres` / `cabal test foucl-integration-postgres-tests`).

## Out Of Scope

- Adapter implementation details.
- Postgres schema definition.
- Startup import behavior.
