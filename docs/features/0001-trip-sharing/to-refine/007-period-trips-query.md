# Period Trips Query

## Goal
Return the ordered trips the frontend needs to derive each visible shared user's location over a requested period.

## How this story achieves the goal
This story adds a specialized endpoint that resolves which users are visible through the share-plus-subscription rule, selects the right seed and in-period trips per user, and returns them grouped in a frontend-friendly shape.

## Technical details
- Add `GET /api/v1/trip-sharing/period-trips?start=<iso>&end=<iso>`.
- Resolve visible users as the intersection of:
  - users the authenticated user subscribes to
  - users who share their trips with the authenticated user
- For each visible user, include:
  - the last trip whose `windowStart` is strictly before `start`
  - all trips whose `windowStart` falls within the requested period
- Order trips by `windowStart` ascending inside each user group.
- Return one entry per visible user containing `username` and ordered `trips`.
- Exclude users who have no visible trip before or during the requested period.
- Reject malformed query parameters and invalid periods where the requested bounds do not make sense.

## Tests
- Contract test for the response shape with one entry per visible user.
- Test proving the seed trip before `start` is returned.
- Test proving in-period trips are returned after the seed trip in ascending `windowStart` order.
- Test proving trips are excluded when the share-plus-subscription rule is not satisfied.
- Test proving users with no visible trips in or before the period are excluded.
- Test covering malformed query parameters and invalid period bounds.
