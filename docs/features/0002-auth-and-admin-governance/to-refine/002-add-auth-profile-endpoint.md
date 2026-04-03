# Add Auth Profile Endpoint

## Goal
Expose a dedicated authenticated profile endpoint so the frontend can restore session state on reload and gate routes without resubmitting credentials.

## How this story achieves the goal
This story adds a cookie-authenticated read endpoint for the current session principal. It gives the frontend a stable source of truth for authenticated-user hydration outside the signin flow.

## Technical details
- Add `GET /api/auth/profile`.
- Require a valid authenticated session cookie.
- Do not accept or require a request body.
- Return the same profile model as `POST /api/signin`:
  - `username`
  - `roles`
  - `approved`
- Resolve the response from the authenticated session principal and persisted user state.
- Return `401` when the request is unauthenticated.
- Return `5xx` when the backend cannot resolve the authenticated user state.
- Keep response shape parity with signin so frontend hydration code can share one decoder.

## Tests
- Contract test for `GET /api/auth/profile` success response shape.
- Test proving the returned profile matches the currently authenticated user.
- Test proving the endpoint rejects unauthenticated access with `401`.
- Test proving the endpoint works after a prior successful signin using the issued cookie.
- Regression test proving the response shape stays aligned with `POST /api/signin`.
