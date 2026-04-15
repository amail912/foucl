# 017 Session Repository Contract

## Objective

Define a decision-complete internal session persistence contract so session business logic no longer depends directly on filesystem storage details.

## Scope

- Define a `SessionRepository` interface for session handles, session states, and user-state bindings.
- Refactor session business functions/store construction to depend on `SessionRepository`.
- Keep cookie signing and token verification behavior in existing session/auth logic layers.
- Keep route/controller behavior unchanged.

## Operational Contract

The repository contract must cover these operations:

1. Create persisted session handle.
2. Delete persisted session handle.
3. Create persisted session state.
4. Load persisted session state by token.
5. Update persisted session state by token.
6. Delete persisted session state by token.
7. Create persisted user-state binding.
8. Delete persisted user-state binding by token.
9. Delete all persisted user-state bindings for a user.

Contract-level semantics:

- State load for unknown token returns `NotFound`.
- State update/delete for unknown token returns `NotFound`.
- User-state delete operations are deterministic and idempotent at the business boundary.
- No filesystem paths, directories, or backend-specific data layout details may appear in the interface.

## Error Mapping Contract

Repository errors must be defined in the shared generic `Repository` module and used across backends.

Required shared repository errors:

- `AlreadyExists`
- `NotFound`
- `ReadFailure`
- `WriteFailure`
- `StorageFailure`

Required mapping behavior at session/auth boundary:

- repository storage/read/write failures map to existing technical-error behavior,
- no new client-visible error category is introduced,
- backend-specific error classes must not leak above repository boundary.

## Call-Site Migration Boundary

In scope for refactor to repository dependency:

- session creation path,
- session resolution path,
- single-session revoke path,
- revoke-all-for-session/user path,
- state idle refresh and get-or-create state behavior.

Out of scope:

- Runtime backend selection and wiring (018).
- Filesystem adapter implementation/parity (019).
- Postgres adapter implementation/parity (020/021).
- Startup import behavior (023/024).

## Acceptance Criteria

1. Session business logic compiles against `SessionRepository`.
2. Direct filesystem persistence calls are removed from session business paths.
3. Repository error mapping preserves current session/auth behavior and client-visible semantics.
4. Repository contract is persistence-agnostic and exposes no backend-specific types.
5. No HTTP route, payload, status code, cookie behavior, or message drift is introduced.

## Test Cases And Scenarios

- Build/compile check proving session business code no longer imports filesystem persistence primitives directly.
- Unit tests for repository-error-to-session-error mapping behavior.
- Contract tests for operation semantics:
  - missing state token handling,
  - state update/delete missing-token handling,
  - revoke-all deterministic behavior.

## Dependencies

- Unblocks:
  - 018 Session Backend Selection And Wiring
  - 019 Session Filesystem Adapter Parity
  - 020 Session Postgres Adapter Implementation
  - 021 Session Postgres Parity Verification
