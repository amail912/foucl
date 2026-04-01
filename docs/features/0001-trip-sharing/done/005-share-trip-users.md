# Share Trip Users

## Goal
Let each authenticated user manage the list of users allowed to see their trips.

## How this story achieves the goal
This story introduces a dedicated share-list resource owned by the authenticated user. It keeps sharing state at the user level rather than on individual trip items, which matches the contract.

## Technical details
- Add `GET /api/v1/trip-sharing/shares`.
- Add `POST /api/v1/trip-sharing/shares`.
- Add `DELETE /api/v1/trip-sharing/shares/:username`.
- Persist the share list independently from calendar items and scope it to the authenticated user.
- Validate that target usernames exist before they are added.
- Keep payloads minimal, returning only `username` for list entries.
- Make add and delete behavior idempotent so clients can safely retry list-management actions.

## Tests
- Contract tests for listing, adding, and deleting shared users.
- Test proving duplicate additions do not create duplicate entries.
- Test covering deletion of a username that is not currently shared.
- Test covering invalid or unknown usernames.
- Isolation tests proving a user can only manage their own share list.
