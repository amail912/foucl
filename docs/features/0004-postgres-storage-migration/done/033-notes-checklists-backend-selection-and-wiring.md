# 033 Notes And Checklists Backend Selection And Wiring

## Objective

Introduce per-domain backend selection and runtime wiring for notes and checklists.

## Scope

- Add backend selectors for notes and checklists (`filesystem|postgres`).
- Wire repository/service composition so selected backend is used per domain.
- Require top-level `database` config when a selected backend is `postgres`.
- Keep startup fail-fast behavior for invalid postgres wiring.

## Dependencies And Boundaries

Depends on:

- 032 Notes And Checklists Repository Contracts.

Out of scope:

- Postgres schema/migrations.
- Postgres adapter behavior.
- Startup import behavior.

## Acceptance Criteria

1. Notes/checklists default behavior remains unchanged when selectors are omitted.
2. `postgres` selection requires valid `database` config and fails fast otherwise.
3. Runtime uses only selected backend path per notes/checklists domain.

## Implementation Decisions

- Backend selectors were added as top-level config keys:
  - `noteBackend`
  - `checklistBackend`
- Omitted note/checklist backend values default to `filesystem`.
- Runtime startup wiring now composes notes and checklists repositories explicitly from selected backend constructors and injects them into API controller wiring.
- In this story, `postgres` mode for notes/checklists is intentionally non-fallback:
  - it requires top-level `database` config,
  - it fails fast with explicit "not implemented yet" wiring errors until story 036 delivers Postgres adapters.
- Startup logs now include selected/wiring-ready/wiring-failed entries for note/checklist backends, matching existing domain backend logging behavior.
