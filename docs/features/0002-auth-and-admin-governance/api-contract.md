# Auth And Admin Governance API Contract

## Goal
Define the backend contract needed for frontend-authenticated rendering, session restoration, and admin approval and deletion workflows.

## Authenticated Profile Model
Successful authenticated profile responses should expose the same model across signin and session-refresh flows.

Required fields:
- `username :: String`
- `roles :: Array String`
- `approved :: Boolean`

Current roles for this iteration:
- `admin`
- `member`

`approved = true` means the account may use authenticated application features.

## Signin
### `POST /api/signin`
Authenticates the provided credentials, issues the session cookie, and returns the authenticated profile body.

Request body:
- `username`
- `password`

Success response body:
- `username`
- `roles`
- `approved`

The response body should reflect the authenticated user associated with the newly issued session.

## Authenticated Profile Endpoint
### `GET /api/auth/profile`
Returns the authenticated profile for the current session.

Rules:
- authentication is cookie-based
- no credentials body is accepted or required
- success response shape must match `POST /api/signin`

This endpoint is intended for reload, session refresh, and route gating.

## Admin Governance
Admin governance is split into:
- pending signup handling
- approved-user management

Pending signup handling:
- `GET /api/v1/admin/pending-signups`
- `POST /api/v1/admin/pending-signups/approve`
- `DELETE /api/v1/admin/pending-signups/:username`

Approved-user management:
- `GET /api/v1/admin/users`
- `DELETE /api/v1/admin/users/:username`

Scope rules:
- pending accounts remain in the dedicated pending-signups flow
- the general users list covers approved users only

Minimal user payloads for admin list responses:
- pending signups:
  - `username`
- approved users:
  - `username`
  - `roles`
  - `approved`

## Status Semantics
The frontend needs stable classes of failure for auth and admin flows.

Minimum meanings:
- `401` unauthenticated
- `403` authenticated but not allowed for the operation, including non-admin access to admin endpoints
- `404` resource not found or unknown target username
- `409` conflict when the request is well-formed but cannot be completed because of current resource state
- `5xx` server failure

Routing rule:
- unknown `/api/...` paths return `404` even when unauthenticated (path mismatch is handled before auth guards)
- known protected paths without a valid session return `401`

Recommended auth-specific behavior:
- invalid signin credentials remain `401`
- pending-but-authentic signin remains `403`
- authenticated profile access without a valid session returns `401`

Recommended admin-governance behavior:
- deleting or approving an unknown username returns `404`
- deleting a user in an incompatible state may return `409` if the implementation distinguishes state conflicts

## Test Expectations
- Contract tests for signin success response shape.
- Contract tests proving `GET /api/auth/profile` matches the signin success profile shape.
- Auth tests for unauthenticated `401` on `GET /api/auth/profile`.
- Admin tests for `403` on non-admin access.
- Governance tests for pending list, approve, and delete.
- Governance tests for approved-user list and delete.
- Error-path tests proving `404`, `409` where relevant, and `5xx` behavior remain distinguishable.
