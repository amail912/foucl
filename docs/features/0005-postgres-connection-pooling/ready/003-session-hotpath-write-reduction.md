# 003 Session Hot-Path Write Reduction

## Objective

Reduce avoidable session-state writes performed during authenticated request resolution.

## Scope

- Refine session touch logic used by `requireAuth`/`resolveSession`.
- Skip `UPDATE session-state` when computed `idleExpiresAt` does not change persisted value.
- Preserve current authentication and TTL semantics.

## Implementation Decisions

- Keep existing absolute and idle TTL rules unchanged.
- Keep session resolve flow unchanged in shape (load handle, load state, validity checks).
- Introduce a write short-circuit only on equality of current vs computed idle expiry.
- Do not introduce debounce windows, caches, or new session tables in this story.

## Dependencies And Boundaries

Depends on:

- 002 Repository Cutover To WithResource.

Out of scope:

- joining handle/state reads into one query.
- broader session model redesign.

## Acceptance Criteria

1. Valid authenticated requests continue to return same auth outcomes as before.
2. Session idle-expiry is still refreshed when a value change is required.
3. No-op touches avoid issuing `UPDATE session-state`.
4. Session parity integration tests remain green.
