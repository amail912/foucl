# Counterparty Suggestions Endpoint

## Goal
As a user, I want counterparty autocomplete suggestions while entering transactions, so data entry is faster and more consistent.

## Behavior And Business Rules
- Add endpoint:
  - `GET /api/v1/finance/counterparties/suggest?q=&limit=&direction=&accountId=`
- Query semantics:
  - `q` is required for matching behavior and has minimum length `1`.
  - If `q` is missing or empty, return `{ "items": [] }`.
  - `limit` is optional, defaults to `8`, and max is `20`.
  - `direction` is optional and supports `sent`, `received`, `all`.
  - `accountId` is optional and narrows usage history.
  - Counterparty values are already stored in canonical normalized form (trimmed and lowercased), and suggestion reads use that canonical form directly.
- Ranking precedence:
1. prefix match before contains match
2. higher `usageCount`
3. more recent `lastUsedAt`
4. lexicographically smaller `value` as deterministic tie-break
- `suggestedCategory` returns the strongest historical category slug association for that counterparty, else `null`.

## Data And Contracts
- Response shape:
  - `items[]` with `value`, `usageCount`, `lastUsedAt`, `suggestedCategory`.
- Auth and ownership rules match existing user-scoped finance reads.
- Invalid query parameter values return `400`.

## Technical Details
- Suggestions must derive from the authenticated user's finance history only.
- Use deterministic tie-breaking so repeated calls are stable for equal ranking signals.
- Reconciliation adjustment rows must not contribute to suggestion candidates or ranking statistics.
- Ensure read-path performance is acceptable for interactive typeahead use.

## Testing
- Empty/missing `q` returns empty `items`.
- Prefix matches rank above contains matches.
- Higher usage ranks above lower usage for equal match class.
- More recent usage ranks above older usage for equal match class and usage count.
- Lexicographic tie-break ordering is deterministic when prior ranking signals are equal.
- `limit` default and cap behavior are enforced.
- Direction and account filters narrow result sets correctly.
- `suggestedCategory` returns expected category slug values or `null`, using frequency then recency ordering.

## Rollout And Compatibility
- v1 contract definition for counterparty suggestions under `/api/v1/finance`.
