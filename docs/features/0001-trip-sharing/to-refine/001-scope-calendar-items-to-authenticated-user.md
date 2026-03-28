# Scope Calendar Items To Authenticated User

## Goal
Make calendar items owner-scoped so the existing calendar items API can be safely reused for trips without leaking data between users.

## How this story achieves the goal
This story threads the authenticated principal into agenda reads and writes, stores calendar items under a per-user namespace, and completes the owner-only CRUD behavior the trip contract depends on.

## Technical details
- Change agenda persistence from one global calendar-items directory to a layout scoped by authenticated user.
- Update `GET /api/v1/calendar-items` so it only returns items owned by the authenticated user.
- Update create and update through `POST /api/v1/calendar-items` so they only operate in the authenticated user's namespace.
- Add delete support for calendar items if it is missing from the agenda API family, and keep deletion owner-scoped.
- Use the session principal as the source of truth for ownership; do not accept owner information from the request body.
- Keep item ids opaque to clients and only meaningful inside the owner's storage scope.

## Tests
- Integration test proving two users only see their own calendar items.
- Integration test proving one user cannot update another user's calendar item by id.
- Integration test proving one user cannot delete another user's calendar item by id.
- Unit tests covering per-user storage paths and owner-isolated item listing.
