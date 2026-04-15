# 021 Session Postgres Parity Verification

## Objective

Validate that session behavior remains contract-compatible when `sessionBackend=postgres` with real Postgres-backed execution.

## Scope

- Execute session-focused parity verification against real Postgres-backed repository behavior.
- Verify client-visible session behavior remains aligned with existing expectations at session/auth business/controller boundaries.
- Verify state lifecycle semantics remain compatible for resolve, idle refresh, and revoke flows.
- Verify status/message semantics remain unchanged for covered scenarios.

## Verification Mode

- This story certifies real Postgres runtime integration for session flows using the 020 adapter and 022 schema.
- Mock-only checks are insufficient for completion.

## Scenario Matrix

- Session create and resolve: success and technical failure paths.
- Session resolve: invalid or missing state handling path.
- Session refresh/touch: idle refresh behavior and technical failure paths.
- Session revoke single: success, not found, and technical failure paths.
- Session revoke-all-for-user/session: success and technical failure paths.

## Dependencies And Boundaries

- Depends on:
  - 017 Session Repository Contract
  - 018 Session Backend Selection And Wiring
  - 020 Session Postgres Adapter Implementation

Out of scope:

- Real Postgres integration execution.
- Session Postgres schema definition and migration execution.
- Session Postgres adapter implementation details.
- Startup import behavior.

## Acceptance Criteria

1. Real Postgres-backed parity checks pass for all scenarios in scope.
2. Session create/resolve/refresh/revoke behaviors remain contract-compatible.
3. Status codes and error messages remain compatible with current contract at the business/controller boundary.
4. No new client-visible error category is introduced.
5. Story explicitly documents that real Postgres integration verification is in scope and required.
