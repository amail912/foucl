# Stabilize Calendar Item Delete Contract

## Goal
Provide one stable delete-by-id contract for calendar items so all frontend calendar surfaces can delete both task and trip items without branching on item type.

## How this story achieves the goal
This story makes deletion behavior explicit at the contract level for every stored calendar item. It treats task and trip deletion as the same calendar capability, with owner-only authorization and consistent status meanings.

## Technical details
- Keep calendar-item deletion under `DELETE /api/v1/calendar-items/:id`.
- Support deletion of both task-backed and trip-backed calendar items through the same endpoint.
- Keep authorization owner-only; one user must not be able to delete another user’s calendar item.
- Make delete outcome semantics stable and distinguishable for:
  - unauthenticated access
  - unauthorized or cross-user attempts
  - unknown item id
  - conflict or validation failure when relevant
  - server failure
- Do not require the frontend to know the underlying item subtype before issuing delete.
- Preserve compatibility with existing calendar surfaces that already delete by calendar item id.

## Tests
- Contract test for deleting a task calendar item successfully.
- Contract test for deleting a trip calendar item successfully.
- Test proving unauthenticated delete requests are rejected.
- Test proving one user cannot delete another user’s calendar item.
- Test proving an unknown calendar item id returns the contract’s not-found status.
- Regression test proving delete remains compatible with existing calendar create and list flows.
