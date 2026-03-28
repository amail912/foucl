# Subscribe To Trip Users

## Goal
Let each authenticated user manage the list of users whose shared trips they want to follow.

## How this story achieves the goal
This story adds a subscription list that mirrors share-list mechanics while remaining an independent relation. The period-trips query can then require both subscription and sharing before exposing another user's trips.

## Technical details
- Add `GET /api/v1/trip-sharing/subscriptions`.
- Add `POST /api/v1/trip-sharing/subscriptions`.
- Add `DELETE /api/v1/trip-sharing/subscriptions/:username`.
- Persist subscriptions independently from shares.
- Validate that target usernames exist before they are added.
- Keep payloads minimal, returning only `username` for list entries.
- Make add and delete behavior idempotent so clients can safely retry list-management actions.

## Tests
- Contract tests for listing, adding, and deleting subscriptions.
- Test proving duplicate additions do not create duplicate entries.
- Test covering deletion of a username that is not currently subscribed to.
- Test covering invalid or unknown usernames.
- Tests proving subscription data is independent from share data.
- Isolation tests proving a user can only manage their own subscription list.
