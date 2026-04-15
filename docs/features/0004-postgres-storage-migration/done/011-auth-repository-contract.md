# 011 Auth Repository Contract

## Objective

Define a decision-complete internal auth persistence contract so auth business logic no longer depends directly on filesystem storage details.

## Scope

- Define an `AuthRepository` interface for auth persistence operations only.
- Refactor `Auth` business functions to depend on `AuthRepository`.
- Keep password hashing, username/password validation, signin password checks, and bootstrap-admin role/approval decisions in the `Auth` business layer.
- Keep route/controller behavior unchanged.

## Repository Operations Contract

The repository contract must cover these operations:

1. Create persisted user record.
2. Load persisted user by username.
3. Update persisted user record.
4. Delete persisted user by username.
5. List persisted users.

The contract is intentionally persistence-agnostic and must not expose filesystem-specific paths or directory details.

## Error Contract And Mapping

Define repository-level errors so `Auth` can map them to existing auth outcomes without API drift.

Repository errors must be defined in a shared generic `Repository` module used by all backends.

Required shared repository error classes:

- `AlreadyExists`
- `NotFound`
- `ReadFailure`
- `WriteFailure`
- `StorageFailure`

Required mapping behavior in `Auth`:

- create user conflict -> `UserAlreadyExists`
- missing user in signin -> `InvalidCredentials`
- missing user in admin approve/delete/list operations where currently expected -> `ResourceNotFound` behavior unchanged
- storage/read/write failures -> `TechnicalError` with current technical class semantics preserved

No new client-visible error category may be introduced in this story.

## Call-Site Migration Boundary

In scope for refactor to repository dependency:

- `createUserWithBootstrapAdmin`
- `signinUser`
- `loadAuthenticatedProfile`
- `userExists`
- `isApprovedAdmin`
- `listPendingUsers`
- `listApprovedUsers`
- `approveUser`
- `deletePendingUser`
- `deleteApprovedUser`

Out of scope for this story:

- Runtime backend selection and wiring (handled by 012).
- Filesystem adapter implementation (handled by 013).
- Postgres adapter implementation (handled by 015).
- Startup import behavior (handled by 023/024).

## Acceptance Criteria

1. Auth business logic compiles against `AuthRepository` dependency.
2. Direct filesystem persistence calls are removed from auth business paths.
3. Repository error mapping preserves existing auth behavior and client-visible semantics.
4. No HTTP route, payload, status code, or message drift is introduced.
5. Story output is ready for 012/013/015 implementation without additional contract decisions.

## Test Cases And Scenarios

- Build/compile check proving `Auth` business code no longer imports filesystem persistence primitives directly.
- Unit tests for repository-error-to-auth-error mapping behavior.
- Regression expectation reference: filesystem-mode auth contract parity remains unchanged (execution validated in 013).

## Dependencies

- Consumed by:
  - 012 Auth Backend Selection And Wiring
  - 013 Auth Filesystem Adapter Parity
  - 015 Auth Postgres Adapter Implementation
