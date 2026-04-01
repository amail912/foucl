# Predefined Places Catalog

## Goal
Expose the fixed list of trip places through a dedicated backend endpoint so trip creation, editing, and validation can rely on a stable shared source of truth.

## How this story achieves the goal
This story introduces a small reference-data endpoint for trip places. The backend owns the catalog, returns it in a stable shape, and keeps it separate from user-owned trip items. Later trip stories can then validate `departurePlaceId` and `arrivalPlaceId` against this catalog without duplicating place definitions in multiple places.

## Technical details
- Add `GET /api/v1/trip-places`.
- Return a JSON array where each place contains `name`.
- The place name is the identifier. There is no separate `id` field in this iteration.
- Initialize the catalog with exactly these places: `Paris`, `Le Mesnil`, and `St Clair`.
- Keep the catalog fixed for this iteration. There is no create, update, delete, or admin API for places in this feature.
- Keep names stable because later trip items will reference them and validation depends on them.
- Return places in a stable order so the frontend gets deterministic results across requests.
- Do not return frontend-only fields such as short labels, display groups, icons, or colors.
- Store the catalog in a backend-owned form that is easy to read and update intentionally, such as a dedicated module or a static data file loaded by the server.
- Keep this endpoint independent from trip item storage and sharing data. It is reference data, not user data.
- Require authentication for the endpoint so it stays consistent with the rest of the authenticated calendar surface.

## Tests
- Contract test for `GET /api/v1/trip-places` proving the response is a JSON array of `{ name }` objects.
- Test proving the endpoint requires authentication.
- Test proving the returned names are exactly `Paris`, `Le Mesnil`, and `St Clair`.
- Test proving the returned names are unique.
- Test proving the returned order is stable across repeated requests.
- Regression test proving the endpoint does not include extra fields beyond the documented contract.
