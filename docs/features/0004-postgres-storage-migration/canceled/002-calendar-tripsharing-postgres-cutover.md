# 002 Calendar And Trip Sharing Postgres Cutover

## Status

Canceled.

## Cancellation Reason

This story was too broad because it mixed multiple delivery layers in one unit:

- repository contracts,
- backend selection/wiring,
- filesystem adapter parity,
- Postgres schema/migrations,
- Postgres adapter implementation,
- parity verification,
- startup import behavior.

This coupling made sequencing, testing, and rollback boundaries unclear.

## Replaced By

- [025 Calendar And Trip Sharing Repository Contracts](../ready/025-calendar-tripsharing-repository-contracts.md)
- [026 Calendar And Trip Sharing Backend Selection And Wiring](../ready/026-calendar-tripsharing-backend-selection-and-wiring.md)
- [027 Calendar And Trip Sharing Filesystem Adapter Parity](../ready/027-calendar-tripsharing-filesystem-adapter-parity.md)
- [028 Calendar And Trip Sharing Postgres Schema And Migrations](../ready/028-calendar-tripsharing-postgres-schema-and-migrations.md)
- [029 Calendar And Trip Sharing Postgres Adapter Implementation](../ready/029-calendar-tripsharing-postgres-adapter-implementation.md)
- [030 Calendar And Trip Sharing Postgres Parity Verification](../ready/030-calendar-tripsharing-postgres-parity-verification.md)
- [031 Calendar And Trip Sharing Startup Import](../ready/031-calendar-tripsharing-startup-import.md)
