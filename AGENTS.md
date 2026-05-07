# AGENTS.md

## Overview
This repository (`foucl`) is a Haskell backend with:
- Domain services for auth, session, calendar/trip-sharing, notes/checklists.
- Dual persistence behavior (filesystem + Postgres-backed behavior), with startup migration/import orchestration.
- Filesystem persistence is meant to disappear soon. It is now deprecated and won't be implemented for new storage needs.
- A strong test suite split into unit, integration API, and integration Postgres-migration/parity suites.

This file is for AI agents starting in a fresh context window: use it as the minimum operational map before editing.

## Architecture
Core areas:
- Runtime entrypoint: `app/Main.hs` -> `Lib.runApp`.
- App wiring/config/startup:
  - `src/Lib.hs`
  - `src/Lib/Config.hs`
  - `src/Lib/Startup.hs`
  - `src/Lib/Server.hs`
- Domain modules:
  - Auth: `src/Auth.hs`, `src/AuthRepository.hs`
  - Session: `src/Session.hs`
  - Calendar/Trip-sharing: `src/CalendarRepository.hs`, `src/TripSharingRepository.hs`, `src/AgendaStorage.hs`, `src/TripSharingStorage.hs`
  - Notes/Checklists: `src/NotesChecklistRepository.hs`, `src/NoteCrud.hs`, `src/ChecklistCrud.hs`
- Shared abstractions/helpers:
  - `src/Repository.hs`
  - `src/SqlHelpers.hs`
  - `src/PostgresMigrations.hs`

## Current Ground Truth (Important)
- Backend parsing is performed in `Lib.Config.toAppConfig`.
- SQL/resource helper conventions:
  - Use `tryExcept` for lifting IO exceptions into `ExceptT`.
  - Use `withResourceM` / `withResourceMHandled` for pool-resource flows.
  - Avoid direct `withResource` in domain modules when `ExceptT` helpers exist.
- Exception-flow readability convention:
  - Avoid nested patterns like `tryExcept (Ex.try ...)` that produce `ExceptT e IO (Either ...)` plus manual `Either` deconstruction.
  - Prefer one `tryExcept ioAction mapper` boundary and map exceptions in mapper functions.

## Commands
Canonical local commands:
- Unit tests: `make test`
- Integration API tests (starts sandbox + Postgres): `make integration-test`
- Postgres startup-migration/storage parity tests: `make storage-migration-tests`
- Build executable: `make build`
- Lint: `make lint`

Notes:
- Integration targets manage Docker and a sandbox daemon automatically.
- Run integration targets sequentially, not in parallel.
- Common local ports/targets used by integration workflow:
  - App: `127.0.0.1:8081`
  - Postgres: `127.0.0.1:5432`

## Coding Conventions for Agents
- Prefer `ExceptT`-native control flow over `IO (Either ...)` plumbing.
- Keep repository error semantics stable when refactoring (`AlreadyExists`, `NotFound`, `ReadFailure`, `WriteFailure`, `StorageFailure`).
- Keep SQL/storage wrappers centralized in helpers (`Helpers`) and domain-level mapper functions.
- Preserve current API/test contracts unless the task explicitly changes behavior.
- Prefer small, behavior-preserving edits and re-run tests early.

## Story Delivery Conventions
- For implementation requests tied to a story:
  - Update relevant docs when behavior or API contracts change.
  - Move the implemented story from `ready/` to `done/`.
  - Update the feature backlog.
  - Commit only after required checks pass.
- For all code implementation requests (story or non-story):
  - Create a commit after finishing implementation and validation.
- For all feature/story doc maintenance:
  - Keep each feature backlog aligned with the story folder state (`ready/`, `done/`, `to-refine/`, `canceled/`).
  - Keep canceled stories as auditable docs rather than deleting them.
- Story commit messages should use:
  - `[xxxx-yyy] Functional description of the story`

## Common Pitfalls / Regression Traps
- Export wiring regressions:
  - Removing/reworking symbols in `Lib.Config` and `Lib` can break broad unit tests quickly.
- Error mapping regressions:
  - Refactors in filesystem adapters (Auth/Session) can silently alter expected `RepositoryError` outcomes.
- Integration environment assumptions:
  - If Docker containers/network already exist or tests are run in parallel, target startup can fail due to name/network conflicts.
- Test fixture validity:
  - Keep generated test fixture values within existing validation constraints (for example username length), or failures may surface far from the real cause.
- Over-eager abstraction:
  - New wrappers are not always desirable; prefer existing helpers unless abstraction materially improves clarity without semantic risk.

## Migration Hygiene
- When adding domain migrations, update all affected migration touchpoints:
  - Migration registration/wiring.
  - Test database reset/rollback flows.
  - Migration-count and seeded-migration assertions in integration tests.

## Validation Checklist After Repository/Storage Refactors
Run, in order:
1. `make test`
2. `make integration-test`
3. `make storage-migration-tests`

Also verify no leftover anti-patterns where applicable:
- `tryExcept (Ex.try ...` in refactored modules.
- Legacy local IO-lifting wrappers still present after migration to `tryExcept` (if in scope for the change).

## Suggested Agent Workflow (Fresh Context Window)
1. Read `Makefile` and `foucl.cabal` to understand runnable targets and test suites.
2. Inspect touched domain modules + nearby helpers before editing.
3. Apply the smallest safe change that preserves behavior.
4. Validate with the narrowest relevant target first, then full required targets.
5. Report exact commands run and concrete outcomes.

## Recent Important Decisions Captured
- Moved toward consistent `ExceptT` patterns in repository modules.
- Standardized on `Helpers` (`tryExcept`, `withResourceM*`) for IO/resource exception flow.
- Kept startup migration domain selection function available via `Lib` for tests/wiring.
- Leave `app-config.json` and `config/app-config-fs.json` untouched unless the user explicitly asks to change them.
