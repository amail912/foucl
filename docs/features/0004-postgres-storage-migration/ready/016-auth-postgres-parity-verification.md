# 016 Auth Postgres Parity Verification

## Objective

Validate that auth behavior remains contract-compatible when `authBackend=postgres`, using mocked repository checks.

## Scope

- Execute auth-focused parity verification with mocked Postgres repository behavior.
- Verify client-visible behavior remains aligned with existing expectations at auth business/controller boundaries.
- Verify admin governance and approval behaviors remain compatible.
- Verify status/message semantics remain unchanged for covered scenarios.

## Verification Mode

- This story uses mocked repository checks only.
- It does not certify real Postgres runtime integration.

## Scenario Matrix

- Signup: success and conflict.
- Signin: success, invalid credentials, pending approval.
- Auth profile load: success and technical failure path.
- Admin pending and approved list operations: success and technical failure paths.
- Admin approve/delete operations: success, not found, conflict (where applicable).

## Acceptance Criteria

1. Mocked parity checks pass for all scenarios in scope.
2. Signup, signin, profile, pending/approved admin operations, and deletion semantics remain behavior-compatible.
3. Status codes and error messages remain compatible with current contract at the business/controller boundary.
4. No new client-visible error category is introduced.
5. Story explicitly documents that real Postgres integration verification is out of scope.

## Out Of Scope

- Real Postgres integration execution.
- Postgres schema definition.
- Postgres adapter implementation details.
- Startup import behavior.
