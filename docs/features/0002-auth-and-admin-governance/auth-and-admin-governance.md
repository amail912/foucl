# Auth And Admin Governance

This backlog captures the backend work needed to expose an authenticated profile contract, close admin governance gaps, and normalize authorization semantics expected by the frontend.

## Goal
Let the frontend render authenticated state immediately after signin, restore authenticated state on reload, and manage approvals and user deletion through a stable admin API.

## Scope For This Iteration
- `POST /api/signin` returns the authenticated profile payload in addition to issuing the session cookie.
- The backend exposes a dedicated cookie-authenticated profile endpoint.
- The backend exposes the admin governance operations needed for pending approvals and approved-user deletion.
- Auth and admin endpoints use stable, frontend-usable status semantics for common failure modes.

## Backend API Contract
- [API Contract](api-contract.md) - Backend endpoints, payload expectations, and status semantics for auth profile and admin governance.

## Stories

### Ready

### To Refine
- [002 Add Auth Profile Endpoint](to-refine/002-add-auth-profile-endpoint.md) - Add a cookie-based endpoint that returns the current authenticated profile.
- [003 Admin Pending User Deletion](to-refine/003-admin-pending-user-deletion.md) - Let admins delete pending signups in the same governance flow as approval.
- [004 Admin Approved User Management](to-refine/004-admin-approved-user-management.md) - Let admins list and delete approved users.
- [005 Normalize Auth Admin Error Semantics](to-refine/005-normalize-auth-admin-error-semantics.md) - Make auth and admin endpoints return stable frontend-usable status classes.

### Done
- [001 Return Profile On Signin](done/001-return-profile-on-signin.md) - Return the authenticated profile body on successful signin with implementation-ready contract detail.
