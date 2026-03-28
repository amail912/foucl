# Calendar Trip Sharing API Contract

## Goal
Define the backend contract needed to support trip sharing and frontend-derived location cues without storing sharing state on individual trips.

## Core Rules
- Trips are stored as dedicated calendar items through the existing calendar items API family.
- Places come from a fixed predefined catalog.
- Sharing is user-based:
  - each user has a list of users they share their trips with
  - each user has a list of users they subscribe to
- When the authenticated user requests shared trips, the backend only returns trips from users who satisfy both conditions:
  - the authenticated user subscribes to them
  - they share their trips with the authenticated user
- The backend does not store any sharing state on a trip item.

## Places
### `GET /api/v1/trip-places`
Returns the fixed list of places available for trips.

Response fields per place:
- `id`
- `name`

No short label is returned by the backend.

## Trip Items
Trips reuse the existing calendar item endpoints.

### Read
- `GET /api/v1/calendar-items`

### Create
- `POST /api/v1/calendar-items`

### Update
- `POST /api/v1/calendar-items`

Relevant fields for trip items:
- common calendar item fields reused by trips:
  - `windowStart`
  - `windowEnd`
- trip-specific fields:
  - `type = "trip"`
  - `departurePlaceId`
  - `arrivalPlaceId`

Validation rules:
- `departurePlaceId` must exist in `trip-places`
- `arrivalPlaceId` must exist in `trip-places`
- `departurePlaceId != arrivalPlaceId`
- `windowEnd` must be strictly after `windowStart`
- only the owner can create, update, or delete their trip items

## Sharing Lists
The backend should expose two independent user lists for the authenticated user.

### Share List
- `GET /api/v1/trip-sharing/shares`
- `POST /api/v1/trip-sharing/shares`
- `DELETE /api/v1/trip-sharing/shares/:username`

This list contains the users the authenticated user allows to see their trips.

### Subscription List
- `GET /api/v1/trip-sharing/subscriptions`
- `POST /api/v1/trip-sharing/subscriptions`
- `DELETE /api/v1/trip-sharing/subscriptions/:username`

This list contains the users the authenticated user wants to follow.

Minimal user payload for both lists:
- `username`

## Period Trips Endpoint
### `GET /api/v1/trip-sharing/period-trips?start=<iso>&end=<iso>`
Returns the ordered trips needed by the frontend to derive each shared user's location for the requested period.

Filtering rule per followed user:
- include the last trip whose `windowStart` is strictly before `start`
- include all trips whose `windowStart` is within the requested period
- only include trips for users allowed by the share-plus-subscription rule described above

Ordering rule:
- trips are ordered by `windowStart` ascending inside each user group

Recommended response shape:
- one entry per shared user
- each entry contains:
  - `username`
  - ordered `trips`

This endpoint should be specialized for shared-trip consultation instead of overloading the generic calendar items response.

## Why The Period Trips Contract Works
The contract is sufficient for frontend presence derivation under the current domain rules.

It works because:
- the seed trip before `start` gives the frontend the latest known trip context entering the period
- if that seed trip is already finished before `start`, the frontend can start from its arrival place
- if that seed trip is still in progress at `start`, the frontend can show the user as in transit until the trip ends
- trips starting later in the period then update the derived state in order

This contract becomes insufficient if later rules allow:
- overlapping trips for the same user
- non-trip events that change a user's location
- ambiguous ordering rules between trips

The backend contract should therefore explicitly guarantee:
- no overlapping trips for a given user, or a deterministic equivalent rule
- location changes only through trips
- ordering by `windowStart`

## Frontend Derivation Rules
The frontend should derive presence over the requested period as follows:
- if no seed trip exists for a user, location is unknown until the first in-period trip provides context
- if the seed trip ended before `start`, initial location is the seed trip arrival place
- if the seed trip is active at `start`, initial state is in transit from departure to arrival until its end
- each later trip updates the displayed state in chronological order
- after a trip ends, the user is considered at the arrival place until another trip changes that state

## Test Expectations
- Contract tests for `GET /api/v1/trip-places`
- Contract tests for trip create and update through `calendar-items`
- Validation tests for invalid places, identical departure and arrival, and invalid time windows
- Authorization tests for trip ownership
- Contract tests for share and subscription list management
- Period-trips tests covering:
  - seed trip returned before `start`
  - in-period trips returned after the seed trip
  - trips excluded when the follow-plus-share rule is not satisfied
  - ascending order by `windowStart`
  - no result for users with no visible trips in or before the period
