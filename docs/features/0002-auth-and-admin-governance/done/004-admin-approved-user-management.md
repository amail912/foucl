# Admin Approved User Management

## Goal
Let admins list approved users and delete one approved user when governance requires removing access.

## How this story achieves the goal
This story introduces a dedicated admin resource for approved-user management. It keeps pending moderation in the existing pending-signups flow and gives the frontend a stable contract for listing approved accounts and deleting other approved accounts when access must be revoked.

## Technical details
- Add `GET /api/v1/admin/users`.
- Add `DELETE /api/v1/admin/users/:username`.
- Keep the endpoints behind the approved-admin authorization gate.
- Limit `GET /api/v1/admin/users` to approved accounts only.
- Keep pending moderation under `/api/v1/admin/pending-signups`; do not include pending users in this resource.
- Return list entries using the existing authenticated-profile shape:
  - `username`
  - `roles`
  - `approved`
- Return `approved = true` for every item in this resource.
- Do not add a request body to either route.
- Implement deletion as physical removal of the approved user’s persisted storage directory under the existing user storage root.
- Return `404` when:
  - the username does not exist
  - the username exists but is not in the approved-user scope, including pending users
- Return `403` for authenticated non-admin users.
- Return `401` for unauthenticated requests.
- Return `409` when:
  - the target username is the configured bootstrap admin
  - the target username is the currently authenticated admin user
- Return `5xx` for storage failures.
- Do not introduce a soft-delete or tombstone model in this story.
- Do not change signin, signout, profile, or pending-signup endpoints in this story.

## Tests
- Contract test for listing approved users.
- Test proving approved users appear with `username`, `roles`, and `approved = true`.
- Test proving pending users do not appear in `GET /api/v1/admin/users`.
- Contract test for deleting an approved user successfully.
- Test proving a deleted approved user no longer appears in the admin users list.
- Test proving a deleted approved user can no longer authenticate afterward.
- Test proving deletion of an unknown approved username returns `404`.
- Test proving deletion of a pending user through this route returns `404`.
- Test proving non-admin access returns `403`.
- Test proving unauthenticated access returns `401`.
- Test proving bootstrap-admin deletion returns `409`.
- Test proving self-deletion returns `409`.
