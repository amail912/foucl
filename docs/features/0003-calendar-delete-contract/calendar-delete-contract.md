# Calendar Delete Contract

This backlog captures the backend work needed to keep calendar-item deletion stable and frontend-compatible across task and trip surfaces.

## Goal
Let the frontend delete calendar items by id through one stable contract regardless of whether the item is a task or a trip.

## Scope For This Iteration
- The backend keeps one delete-by-id contract for calendar items.
- Task and trip deletion follow the same owner-only authorization model.
- Delete responses use stable, distinguishable status classes for the frontend.

## Stories

### To Refine

### Done
- [001 Stabilize Calendar Item Delete Contract](done/001-stabilize-calendar-item-delete-contract.md) - Make calendar-item deletion consistent and explicit across task and trip usage with an implementation-ready contract.
