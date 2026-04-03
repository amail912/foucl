# Return Profile On Signin

## Goal
Return the authenticated profile payload immediately on successful signin so the frontend can render role-aware state without making a second request.

## How this story achieves the goal
This story changes the signin success contract from cookie-only session establishment to cookie plus profile delivery. It keeps `POST /api/signin` as the single entry point for both authentication and initial frontend hydration, while leaving the separate session-refresh endpoint to story `0002-002`.

## Technical details
- Keep `POST /api/signin` as the signin endpoint and keep the request body unchanged:
  - `username`
  - `password`
- Keep the current signin flow:
  - validate credentials
  - reject pending accounts
  - create the authenticated session
  - set the signed session cookie
- Change only the success response body from an empty object to the shared authenticated profile model.
- Return this JSON payload on successful signin:
  - `username :: String`
  - `roles :: Array String`
  - `approved :: Boolean`
- Derive the success payload from the persisted authenticated user record already used by authorization decisions:
  - `username` is the authenticated username
  - `roles` is derived from the stored role using the current wire values such as `admin` and `member`
  - `approved` is derived from the stored approval status
- Keep session cookie behavior unchanged:
  - same cookie name source
  - same signature mechanism
  - same `HttpOnly`, `SameSite=Lax`, and configured `Secure` behavior
- Keep failure semantics unchanged:
  - `401` for invalid credentials
  - `403` for pending users
  - `5xx` for technical failures
- Do not introduce a new endpoint in this story.
- Do not change signup, signout, admin endpoints, or the future `GET /api/auth/profile` work in this story.
- The implementation must expose enough persisted-user data from the successful signin path for the HTTP layer to serialize the profile response without re-inventing role or approval logic in the controller.

## Tests
- Contract test for signin success response shape.
- Test proving an approved member signin returns:
  - the authenticated `username`
  - `roles` containing `member`
  - `approved = true`
- Test proving an approved admin signin returns `roles` containing `admin`.
- Regression test proving successful signin still sets the session cookie with the existing attributes.
- Regression test proving invalid credentials still return `401`.
- Regression test proving pending users still return `403` with the existing message.
- Do not add `GET /api/auth/profile` tests here; those belong to story `0002-002`.
