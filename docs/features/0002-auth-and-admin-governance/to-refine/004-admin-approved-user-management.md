# Admin Approved User Management

## Goal
Let admins list approved users and delete one approved user when governance requires removing access.

## How this story achieves the goal
This story introduces a separate admin resource for approved-user management. It keeps pending moderation in the existing pending-signups flow and gives the frontend a stable contract for approved-account administration.

## Technical details
- Add `GET /api/v1/admin/users`.
- Add `DELETE /api/v1/admin/users/:username`.
- Limit the users list to approved accounts.
- Return minimal list entries containing:
  - `username`
  - `roles`
  - `approved`
- Keep the endpoints behind the approved-admin authorization gate.
- Return `404` when the target username does not exist in the approved-user scope.
- Return `403` for authenticated non-admin users.
- Return `401` for unauthenticated requests.
- Return `5xx` for storage failures.
- Do not fold pending accounts into this resource; pending moderation remains under `pending-signups`.

## Tests
- Contract test for listing approved users.
- Test proving pending users do not appear in `GET /api/v1/admin/users`.
- Contract test for deleting an approved user successfully.
- Test proving a deleted approved user no longer appears in the admin users list.
- Test proving a deleted approved user can no longer authenticate afterward.
- Test proving deletion of an unknown approved username returns `404`.
- Test proving non-admin access returns `403`.
- Test proving unauthenticated access returns `401`.
