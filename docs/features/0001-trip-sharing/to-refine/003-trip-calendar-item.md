# Trip Calendar Item

## Goal
Allow trips to be represented as calendar items through the existing calendar items API family.

## How this story achieves the goal
This story extends the agenda model and JSON contract so the backend can accept, persist, and return trip items using the fields defined in the trip-sharing contract.

## Technical details
- Extend the calendar item model to support trip payloads with `type = "trip"`.
- Support trip-specific fields `departurePlaceId` and `arrivalPlaceId` alongside the shared time-window fields `windowStart` and `windowEnd`.
- Reconcile the current agenda wire format with the new public contract so the API exposed to the frontend matches the documented trip payload shape.
- Ensure trip items round-trip correctly on create, list, and update.
- Preserve existing non-trip calendar item behavior unless the implementation explicitly deprecates it as part of the same change.

## Tests
- Contract test for creating a trip item through `POST /api/v1/calendar-items`.
- Contract test for updating a trip item through `POST /api/v1/calendar-items`.
- Contract test for listing trip items through `GET /api/v1/calendar-items`.
- Regression tests covering any existing non-trip calendar item behavior that remains supported.
