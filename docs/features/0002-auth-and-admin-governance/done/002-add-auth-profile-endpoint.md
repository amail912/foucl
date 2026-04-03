# Add Auth Profile Endpoint

## Goal
Expose a dedicated authenticated profile endpoint so the frontend can restore session state on reload and gate routes without resubmitting credentials.

## How this story achieves the goal
This story adds a cookie-authenticated read endpoint for the current session principal. It reuses the same public profile contract as signin so the frontend can hydrate authenticated state from either the initial signin response or a later session-refresh request.

## Technical details
- Add `GET /api/auth/profile`.
- Place the route under `/api/auth/profile`, not under `/api/v1`.
- Require a valid authenticated session cookie and rely on the existing session-resolution path.
- Do not accept or require a request body.
- Return the same profile model already exposed by `POST /api/signin`:
  - `username :: String`
  - `roles :: Array String`
  - `approved :: Boolean`
- Reuse the existing authenticated profile response shape and serialization path instead of creating a second profile wire format.
- Resolve the response from:
  - the authenticated session principal resolved from the cookie
  - a persisted-user lookup for that principal
- Do not revalidate credentials in this endpoint.
- Return `401` when the request is unauthenticated or the session cookie is invalid.
- Return `5xx` when the session principal is valid but the backend cannot resolve the persisted authenticated user state.
- Do not use `404` as a normal response for this endpoint; missing persisted user state behind a valid session is a backend inconsistency.
- Do not change signin, signout, or admin endpoints in this story.
- Keep normal session behavior unchanged apart from the standard authenticated-session resolution already performed by `requireAuth`.

## Tests
- Contract test for `GET /api/auth/profile` success response shape.
- Test proving the endpoint returns the same profile payload as a prior successful signin for the same user.
- Test proving the endpoint works with the session cookie issued by signin.
- Test proving the endpoint rejects unauthenticated access with `401`.
- Regression test proving the response shape stays aligned with `POST /api/signin`.
- If the implementation introduces a dedicated principal-to-profile lookup helper, add unit coverage for that mapping path.
