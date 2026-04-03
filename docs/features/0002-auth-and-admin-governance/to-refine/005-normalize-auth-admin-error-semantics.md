# Normalize Auth Admin Error Semantics

## Goal
Make auth and admin endpoints return stable, distinguishable status classes that the frontend can use for route gating, governance flows, and user-facing error handling.

## How this story achieves the goal
This story aligns the backend’s auth and admin endpoints around a shared error vocabulary. It does not change the business capabilities by itself; it makes those capabilities predictable for frontend state handling.

## Technical details
- Audit auth and admin endpoints against the required semantics:
  - `401` unauthenticated
  - `403` authenticated but not allowed
  - `404` unknown resource
  - `409` conflict when relevant
  - `5xx` server failure
- Keep invalid signin credentials as `401`.
- Keep pending-user signin as `403`.
- Ensure admin moderation and user-management endpoints distinguish unknown target usernames with `404`.
- Use `409` only where the implementation needs to signal a valid request that conflicts with current resource state.
- Keep error responses frontend-usable, preferably with stable JSON message payloads where the existing API already returns them.
- Avoid collapsing all admin failures into `400` or `500` when the frontend needs actionable distinctions.

## Tests
- Endpoint-level tests proving unauthenticated auth/profile and admin requests return `401`.
- Endpoint-level tests proving authenticated non-admin requests return `403`.
- Tests proving unknown moderation targets return `404`.
- Tests proving state-conflict paths return `409` when implemented.
- Regression tests proving server/storage failures still surface as `5xx`.
