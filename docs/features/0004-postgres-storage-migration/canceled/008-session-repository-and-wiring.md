# 008 Session Repository And Wiring

## Status

Canceled.

## Cancellation Reason

This story mixed multiple responsibilities into a single implementation unit:

- session repository contract definition,
- session backend selection and startup wiring,
- filesystem parity expectations.

That coupling made implementation and verification boundaries less clear.

## Replacement Stories

This story is replaced by:

- 017 Session Repository Contract
- 018 Session Backend Selection And Wiring
- 019 Session Filesystem Adapter Parity

## Notes

The replacement split mirrors the proven auth migration granularity (contract -> wiring -> filesystem adapter parity) to keep stories implementable, testable, and dependency-safe.
