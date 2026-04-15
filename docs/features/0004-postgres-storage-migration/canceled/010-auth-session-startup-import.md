# 010 Auth Session Startup Import

## Status

Canceled.

## Cancellation Reason

This story mixed multiple responsibilities into one implementation unit:

- auth startup import execution and verification,
- session startup import execution and verification.

That coupling reduced implementation and testing clarity across two domains with distinct persistence shapes.

## Replacement Stories

This story is replaced by:

- 023 Auth Startup Import
- 024 Session Startup Import

## Notes

The replacement split keeps startup import ownership domain-specific while preserving shared migration principles (gating by backend flag, deterministic upsert, warning-on-overlap, idempotent reruns).
