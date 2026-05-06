# Unify Finance Event Log Foundation

## Goal
As a backend maintainer, I want one canonical finance event log, so ordering, replay, and export can rely on a single deterministic source of truth.

## Behavior And Business Rules
- Finance event history is represented in one canonical append-only store: `finance_events`.
- Global event ordering is deterministic and based on `event_number`.
- Canonical ordering does not depend on timestamp precision or projection query behavior.
- This story does not change external finance endpoint behavior.

## Data And Contracts
- Introduces canonical table `finance_events` with required envelope fields:
  - `event_number` (global monotonic ordering key)
  - `event_id`
  - `user_id`
  - `stream_id`
  - `stream_version`
  - `event_type`
  - `event_version`
  - `occurred_at`
  - `recorded_at`
  - `idempotency_key` (nullable)
  - `payload`
- Enforces invariants needed by later stories:
  - append-only semantics
  - uniqueness of `event_id`
  - uniqueness of `(stream_id, stream_version)`
- Existing public API contracts remain unchanged in this story.
- Existing projection tables remain available and unchanged for active read contracts.

## Technical Details
- Reset-baseline assumption applies: this story establishes the canonical schema as the finance event foundation without legacy compatibility requirements.
- Update finance migration chain to use canonical-event baseline artifacts.
- Update migration registration and migration-count assertions to reflect the new finance migration set.
- Keep canonical envelope naming aligned with `data-model.md` and `api-contract.md` event-envelope intent.
- Explicitly out of scope:
  - write-path cutover to canonical append
  - export source cutover
  - projection replay cutover

## Testing
- Migration tests verify canonical finance schema creation with required constraints and indexes.
- Storage-level tests verify:
  - deterministic `event_number` ordering
  - `event_id` uniqueness
  - `(stream_id, stream_version)` uniqueness
- Startup migration integration tests are updated to the new finance migration list.
- Existing non-finance migration and startup tests remain green.

## Rollout And Compatibility
- Backward-compatible for API consumers.
- Internal storage foundation is introduced in this step.
- No external contract cutover occurs in this story.
