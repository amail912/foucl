# Reconciliation Adjustment Workflow

## Goal
As a user, I want unsupported balance differences to be captured as first-class reconciliation adjustments, so my finance history stays auditable without misclassifying corrections as ordinary spending or income.

## Problem Context
- Story `009` adds trusted snapshot basis selection, but some balance differences are not ordinary transactions.
- We need a separate adjustment workflow that models those corrections explicitly.
- Adjustment behavior may affect ledger, report, and export visibility, so it needs its own design pass.

## Questions To Refine
- Is an adjustment a new event type, a new transaction-like entity, or both?
- Does an adjustment affect ledger balances, report aggregates, or only reconciliation views?
- How is an adjustment represented in export and current snapshot reads?
- Can adjustments be reversed or superseded?
- Which account or snapshot does an adjustment attach to?

## Expected Outcomes
- A first-class adjustment workflow with explicit API and event semantics.
- Clear separation between normal transactions and reconciliation adjustments.
