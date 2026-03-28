# Trip Validation And Editing

## Goal
Keep trip creation, updates, and deletion valid, understandable, and safe for the later shared-period query.

## How this story achieves the goal
This story adds the trip-specific validation rules required by the contract and enforces deterministic per-user trip data so the period query can return an ordered, unambiguous trip history.

## Technical details
- Validate that `departurePlaceId` exists in the trip places catalog.
- Validate that `arrivalPlaceId` exists in the trip places catalog.
- Validate that `departurePlaceId` and `arrivalPlaceId` are different.
- Validate that `windowEnd` is strictly after `windowStart`.
- Enforce the backend guarantee needed by the period query by rejecting overlapping trips for the same user.
- Keep create, update, and delete owner-only, with no cross-user editing or deletion path.
- Return clear client-facing validation failures for invalid trip payloads instead of accepting bad state into storage.

## Tests
- Validation tests for unknown departure and arrival places.
- Validation tests for identical departure and arrival places.
- Validation tests for invalid time windows where `windowEnd` is not after `windowStart`.
- Validation tests for overlapping trips belonging to the same user.
- Authorization tests proving only the owner can edit or delete a trip item.
