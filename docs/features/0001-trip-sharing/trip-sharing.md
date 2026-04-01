# Calendar Trip Sharing

This backlog captures the backend work needed to support trip sharing, controlled visibility between users, and the specialized trip queries the frontend uses to derive presence over time.

## Goal
Let users share trip information in a controlled way so other users can retrieve the right ordered trips to understand where someone is over time from the calendar.

## Scope For This Iteration
- Trips are dedicated calendar items that reuse the existing calendar items API family.
- Places come from a fixed predefined catalog exposed by the backend.
- Shared visibility is resolved from user-level share and subscription lists handled by the backend.
- The backend exposes a specialized period-trips endpoint so the frontend can derive presence over a requested period.
- The backend does not store any sharing state on individual trip items.

## Backend API Contract
- [API Contract](api-contract.md) - Backend endpoints, payload expectations, and frontend derivation assumptions.

## Stories

### To Refine
- [006 Subscribe To Trip Users](to-refine/006-subscribe-to-trip-users.md) - Let users manage whose trips they want to follow.
- [007 Period Trips Query](to-refine/007-period-trips-query.md) - Return the ordered trips needed for frontend presence derivation.

### Done
- [001 Scope Calendar Items To Authenticated User](done/001-scope-calendar-items-to-authenticated-user.md) - Isolate calendar item reads and writes by owner before trips reuse the agenda API.
- [002 Predefined Places Catalog](done/002-predefined-places-catalog.md) - Expose the fixed list of allowed trip places.
- [003 Trip Calendar Item](done/003-trip-calendar-item.md) - Extend calendar items so they can represent trip payloads in the new contract.
- [004 Trip Validation And Editing](done/004-trip-validation-and-editing.md) - Keep trip creation, updates, and deletion valid and deterministic.
- [005 Share Trip Users](done/005-share-trip-users.md) - Let users manage who may see their trips.
