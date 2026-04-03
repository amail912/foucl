# Admin Pending User Deletion

## Goal
Let admins delete pending signups as part of the same governance flow that already lists and approves them.

## How this story achieves the goal
This story completes the pending-signup moderation loop. Instead of forcing admins to approve or leave a pending account in place, it adds a direct deletion path for rejected or unwanted pending users.

## Technical details
- Add `DELETE /api/v1/admin/pending-signups/:username`.
- Keep the endpoint behind the same approved-admin authorization gate as the existing pending-signup list and approve operations.
- Restrict this deletion flow to pending accounts only.
- Implement deletion by removing the pending user’s persisted storage directory under the existing user storage root.
- Do not introduce a tombstone or soft-delete state for this story.
- Keep `GET /api/v1/admin/pending-signups` and `POST /api/v1/admin/pending-signups/approve` unchanged as the surrounding moderation flow.
- Return `200` with the current empty success response pattern when a pending signup is deleted successfully.
- Return `404` when:
  - the username does not exist
  - the username exists but is not pending
- Return `403` for authenticated non-admin users.
- Return `401` for unauthenticated requests.
- Return `5xx` for storage read or deletion failures.
- Do not extend this route to approved-user deletion; approved-user management remains part of story `0002-004`.

## Tests
- Contract test for deleting a pending signup successfully.
- Test proving a deleted pending user no longer appears in the pending signup list.
- Test proving a deleted pending user can no longer sign in.
- Test proving approving a deleted pending user returns the route’s not-found outcome.
- Test proving deletion of an unknown pending username returns `404`.
- Test proving deletion of an approved user through this pending-only route returns `404`.
- Test proving non-admin access returns `403`.
- Test proving unauthenticated access returns `401`.
