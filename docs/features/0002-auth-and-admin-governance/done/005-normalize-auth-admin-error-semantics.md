# Normalize Auth Admin Error Semantics

## Goal
Make auth and admin endpoints return stable, distinguishable status classes that the frontend can use for route gating, governance flows, and user-facing error handling.

## How this story achieves the goal
This story aligns the backend’s auth and admin endpoints around a shared error vocabulary. It does not add new business capabilities; it makes existing auth and governance flows predictable for frontend state handling.

## Technical details
- Limit this story to:
  - `POST /api/signin`
  - `GET /api/auth/profile`
  - `GET /api/v1/admin/pending-signups`
  - `POST /api/v1/admin/pending-signups/approve`
  - `DELETE /api/v1/admin/pending-signups/:username`
  - `GET /api/v1/admin/users`
  - `DELETE /api/v1/admin/users/:username`
- Normalize those endpoints around these status classes:
  - `401` unauthenticated or invalid signin credentials
  - `403` authenticated but not allowed
  - `404` unknown target resource or target outside the route’s resource scope
  - `409` conflict when the request is valid but blocked by current resource state
  - `5xx` server or storage failure
- Keep invalid signin credentials as `401`.
- Keep pending-user signin as `403`.
- Keep admin moderation and user-management endpoints distinguishing unknown target usernames with `404`.
- Require `POST /api/v1/admin/pending-signups/approve` to return `404`, not `401`, when the target username does not exist.
- Keep `409` limited to approved-user deletion conflict paths already modeled by the backend.
- Return error bodies for all in-scope auth/admin failures as stable JSON messages of the form `{ "message": "<string>" }`.
- Preserve existing established message strings where they already exist:
  - `Invalid credentials`
  - `Account pending approval`
  - `Not authenticated`
  - `Admin privileges required`
  - `Not found`
- Normalize remaining in-scope `5xx` branches that still return empty bodies so they also return stable JSON messages.
- Do not change success payloads, request shapes, or non-auth/admin endpoints in this story.
- Do not broaden this story into request-validation `400` semantics beyond preserving existing decode and validation behavior.

## Tests
- Endpoint-level tests proving unauthenticated auth/profile and admin requests return `401` with JSON message bodies.
- Endpoint-level tests proving authenticated non-admin requests return `403` with JSON message bodies.
- Tests proving invalid signin credentials remain `401`.
- Tests proving pending-user signin remains `403`.
- Test proving `POST /api/v1/admin/pending-signups/approve` returns `404` for an unknown username.
- Tests proving pending-signup and approved-user delete routes return `404` for out-of-scope usernames.
- Tests proving approved-user delete conflict paths still return `409`.
- Regression tests proving in-scope technical-failure paths return `5xx` with JSON message bodies rather than empty responses.
