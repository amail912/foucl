# Scope Calendar Items To Authenticated User

## Goal
Ensure the calendar items API is owner-scoped so an authenticated user can only list, create, update, validate, and delete their own calendar items.

## How this story achieves the goal
This story introduces ownership boundaries into the agenda flow. It uses the authenticated session principal as the source of truth for item ownership, scopes calendar-item storage by user, and updates agenda handlers so all calendar-item operations run inside the authenticated user's namespace instead of the current global one.

## Technical details
- Thread `principalUserId` from the authenticated `AppContext` into all agenda read and write operations.
- Replace the current global calendar-item storage layout with a per-user layout under the calendar-items storage root.
- Update `GET /api/v1/calendar-items` so it only returns items belonging to the authenticated user.
- Update create through `POST /api/v1/calendar-items` so new items are always written into the authenticated user's namespace.
- Update item update through `POST /api/v1/calendar-items` with `id` so it only reads and writes inside the authenticated user's namespace.
- Update duration validation through `POST /api/v1/calendar-items` with `ValidateRequest` so it only updates items inside the authenticated user's namespace.
- Add owner-scoped delete support for calendar items if the agenda API does not already expose it, because later trip stories require owner-only deletion.
- Do not add an owner field to the public payload; ownership remains implicit from the authenticated session.
- Keep item ids opaque to clients. The backend may reuse the same id format, but ids are only meaningful within an owner's namespace.

## Tests
- Integration test proving unauthenticated access to calendar items is still rejected.
- Integration test proving two authenticated users only see their own calendar items in `GET /api/v1/calendar-items`.
- Integration test proving one user cannot update another user's calendar item by id.
- Integration test proving one user cannot validate the duration of another user's calendar item by id.
- Integration test proving one user cannot delete another user's calendar item by id.
- Unit tests covering per-user storage path resolution and owner-isolated listing behavior.
- Regression tests proving the existing single-user agenda create/list/update/validate lifecycle still works after ownership scoping is added.
