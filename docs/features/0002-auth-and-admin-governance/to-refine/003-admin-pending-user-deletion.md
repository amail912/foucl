# Admin Pending User Deletion

## Goal
Let admins delete pending signups as part of the same governance flow that already lists and approves them.

## How this story achieves the goal
This story completes the pending-signup moderation loop. Instead of forcing admins to approve or leave a pending account in place, it adds a direct deletion path for rejected or unwanted pending users.

## Technical details
- Add `DELETE /api/v1/admin/pending-signups/:username`.
- Keep the endpoint behind the same approved-admin authorization gate as the other pending-signup operations.
- Restrict this deletion flow to pending accounts only.
- Return `404` when the username does not exist or is not a pending signup.
- Return `403` for authenticated non-admin users.
- Return `401` for unauthenticated requests.
- Return `5xx` for storage failures.
- Keep `GET /api/v1/admin/pending-signups` and `POST /api/v1/admin/pending-signups/approve` as the surrounding moderation flow.

## Tests
- Contract test for deleting a pending signup successfully.
- Test proving a deleted pending user no longer appears in the pending signup list.
- Test proving a deleted pending user can no longer sign in or be approved.
- Test proving deletion of an unknown pending username returns `404`.
- Test proving non-admin access returns `403`.
- Test proving unauthenticated access returns `401`.
