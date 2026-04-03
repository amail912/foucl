# Stabilize Calendar Item Delete Contract

## Goal
Provide one stable delete-by-id contract for calendar items so all frontend calendar surfaces can delete both task and trip items without branching on item type.

## How this story achieves the goal
This story makes deletion behavior explicit at the contract level for every stored calendar item. It treats task and trip deletion as the same calendar capability, with owner-only semantics and stable frontend-usable status meanings.

## Technical details
- Keep calendar-item deletion under `DELETE /api/v1/calendar-items/:id`.
- Support deletion of both task-backed and trip-backed calendar items through the same endpoint.
- Keep auth cookie-based through the existing authenticated calendar API.
- Do not add a request body to this route.
- Keep deletion owner-only by resolving the delete against the authenticated user’s calendar storage only.
- Return `401` for unauthenticated delete requests.
- Return `404` when:
  - the item id does not exist
  - the item exists but belongs to a different user
- Return `5xx` for storage failures.
- Keep `200` with the current empty success response pattern on successful delete.
- Do not introduce `409` or item-type-specific delete behavior in this story.
- Do not require the frontend to know the underlying item subtype before issuing delete.
- Preserve compatibility with existing calendar surfaces that already delete by calendar item id.
- Do not change calendar create, list, update, or validate behavior in this story.

## Tests
- Contract test for deleting a task calendar item successfully.
- Contract test for deleting a trip calendar item successfully.
- Test proving unauthenticated delete requests return `401`.
- Test proving one user cannot delete another user’s calendar item and receives `404`.
- Test proving an unknown calendar item id returns `404`.
- Regression test proving delete remains compatible with existing calendar create and list flows for both task and trip items.
