# 004 Domain Switching And Bootstrap Import

## Status

Canceled as superseded.

## Rationale

This story grouped two broad, cross-domain concerns in one unit:

- per-domain backend switching/runtime wiring,
- filesystem-to-Postgres startup import behavior.

That scope was intentionally delivered through smaller domain-first stories for implementation safety and verification clarity.

## Replaced By

- Backend switching and runtime wiring:
  - 012 Auth Backend Selection And Wiring
  - 018 Session Backend Selection And Wiring
  - 026 Calendar And Trip Sharing Backend Selection And Wiring
  - 033 Notes And Checklists Backend Selection And Wiring
- Startup import behavior:
  - 023 Auth Startup Import
  - 024 Session Startup Import
  - 031 Calendar And Trip Sharing Startup Import
  - 038 Notes And Checklists Startup Import
- Startup migration orchestration and verification:
  - 039 Startup Migration Orchestrator
  - 040 Startup Migration Observability And Failure Contract
  - 041 Startup Migration Integration Verification
  - 042 Runtime Docs Alignment For Startup Migrations

## Outcome

No remaining implementation work is owned by story 004.
