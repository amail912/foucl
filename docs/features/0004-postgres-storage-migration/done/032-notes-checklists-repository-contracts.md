# 032 Notes And Checklists Repository Contracts

## Objective

Define repository contracts for notes and checklists that preserve current CRUD and optimistic version semantics.

## Scope

- Define contract-level read/write/modify/delete behavior for notes and checklists.
- Preserve `StorageId` version semantics used by existing optimistic concurrency checks.
- Keep business/controller expectations explicit and backend-agnostic.

## Dependencies And Boundaries

Depends on:

- 003 Notes And Checklists Postgres Cutover (canceled umbrella; replaced by this split).

Out of scope:

- Backend wiring switches.
- Postgres schema design.
- Postgres adapter implementation.

## Acceptance Criteria

1. Contract behavior for notes/checklists CRUD is explicit and testable.
2. Optimistic version conflict semantics are preserved at contract level.
3. No HTTP contract drift is introduced by contract extraction.

## Implementation Decisions

- A shared contract module was introduced for notes/checklists repositories with domain aliases:
  - `NoteRepository`
  - `ChecklistRepository`
- The contract surface is explicit and backend-agnostic:
  - create item
  - list items
  - delete by id
  - update if current version
- Filesystem-backed adapters are provided as the baseline implementation in this story, preserving current semantics.
- List operations preserve current tolerant behavior by ignoring malformed stored entries instead of failing the entire request.
- Controllers now depend on explicit repository contracts for note/checklist operations while preserving route, payload, and status/message behavior.
