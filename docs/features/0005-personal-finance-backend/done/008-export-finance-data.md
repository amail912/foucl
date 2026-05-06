# Export Finance Data

## Goal
As a user, I want to export my finance data, so I keep ownership of my records and can analyze them outside the application.

## Behavior And Business Rules
- Export returns only the authenticated user's finance data.
- Export must include enough information to preserve an auditable representation of accounts, transactions, links, notes, categories, and snapshots.
- Export must not omit canonical data that would prevent later external analysis of the user's financial history.
- Raw event history is the canonical representation in the export.
- `views` are included for convenience only.
- Delete and retention workflows are out of scope for this story.

## Data And Contracts
- Defines `GET /api/v1/finance/export`.
- Export returns one JSON document with top-level `formatVersion`, `events`, and `views` fields.
- `formatVersion = 1` in the first release.
- The canonical `events` section exports the stored event-envelope representation directly and is partially future-import-compatible.
- `views` includes current accounts, categories, transactions, transfer state, non-deleted notes, and snapshot list.
- `views.transactions` reuse the same row shape as `GET /api/v1/finance/transactions`.
- `views` excludes reconciliation outputs, aggregate report outputs, and deleted notes.
- Unauthenticated export requests return `401`.
- Export-generation failures return `5xx`.

## Technical Details
- Export generation should read canonical stored finance data rather than a lossy report-only view.
- Export implementation should remain compatible with append-only event history and projection-backed reads.
- If the export includes both raw events and convenience views, the contract must identify raw events as canonical.
- Convenience views may evolve independently from the canonical event section.
- This story owns implementation of `GET /api/v1/finance/export`.
- Legacy-storage cleanup and rollout finalization continue in `010d`.

## Testing
- Export returns only authenticated user data.
- Export contains canonical raw events and the expected convenience views.
- `formatVersion` is `1`.
- `views.transactions` match the `GET /api/v1/finance/transactions` row shape.
- Export excludes reconciliation outputs and aggregate report outputs.
- Export preserves deleted-note history only through raw events, not convenience views.
- Unauthenticated export requests return `401`.
- Repeated exports over unchanged data return the same structural shape and `formatVersion`.

## Rollout And Compatibility
- Backward-compatible because this is a new API surface.
- Export format changes after first release should require explicit `formatVersion` evolution.
- Story dependency note:
  - `010d` depends on this story being complete.
