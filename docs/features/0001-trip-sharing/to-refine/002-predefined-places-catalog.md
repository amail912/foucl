# Predefined Places Catalog

## Goal
Expose the fixed list of valid trip places so trip creation and editing can reference a backend-owned source of truth.

## How this story achieves the goal
This story adds a dedicated endpoint backed by a static catalog of places. It keeps place validation and place selection aligned without coupling place data to individual trip items.

## Technical details
- Add `GET /api/v1/trip-places`.
- Return one entry per place with `id` and `name`.
- Keep the place catalog fixed and backend-controlled for this iteration.
- Do not return frontend-specific fields such as short labels or presentation hints.
- Store the catalog in a simple backend-friendly form, such as a static module or config-backed file, as long as ids remain stable.

## Tests
- Contract test for the `GET /api/v1/trip-places` response shape.
- Test proving place ids are unique and stable.
- Test covering the intended authentication behavior of the endpoint.
