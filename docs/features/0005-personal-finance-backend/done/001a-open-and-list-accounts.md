# Open And List Accounts

## Goal
As a user, I want to create and view my finance accounts, so transactions and balances can be attached to explicit account resources.

## Behavior And Business Rules
- Opening an account creates an active account with a user-visible name and an internal id.
- Account names must not be empty or whitespace-only.
- Account names are trimmed before persistence and comparison.
- Account names must be unique per authenticated user using case-insensitive comparison.
- Duplicate names remain invalid even when the existing account is already closed.
- `GET /accounts` returns active accounts by default.
- `GET /accounts?status=closed` returns closed accounts only.
- `GET /accounts?status=all` returns both active and closed accounts.
- Account reads must remain scoped to the authenticated user only.

## Data And Contracts
- Defines `POST /api/v1/finance/accounts` and `GET /api/v1/finance/accounts`.
- `POST /api/v1/finance/accounts` request body contains `name` only in v1.
- `POST /api/v1/finance/accounts` success response returns `id`, `name`, and `status`.
- `GET /api/v1/finance/accounts` returns account rows with `id`, `name`, and `status`.
- Empty or whitespace-only names return `400`.
- Duplicate account names for one user return `409`.

## Technical Details
- Account lifecycle is modeled independently from transaction events.
- Account-list reads should come from a projection rather than event replay on every request.
- The account projection must support default active-only listing and explicit `status` filtering.
- The implementation should use Postgres-backed event storage and projection tables only; no filesystem-backed storage should be introduced.
- Runtime startup now requires `backend=postgres`; filesystem mode is no longer a valid runtime configuration once finance is enabled.

## Testing
- Create account succeeds with a valid name.
- Creating an account with an empty or whitespace-only name returns `400`.
- Creating a second account with the same name for the same user returns `409`.
- Creating a second account with the same trimmed name but different letter case returns `409`.
- Creating a second account with the same name as a closed account returns `409`.
- Accounts with the same name can coexist for different authenticated users only if the wider platform still supports multiple user identities.
- Default account listing returns active accounts only.
- `status=closed` returns only closed accounts.
- `status=all` returns active and closed accounts.

## Rollout And Compatibility
- Backward-compatible because this is a new API surface.
- Any migration should be additive for new account event and projection storage.
