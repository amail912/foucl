INSERT INTO auth_users (username, password_hash, role, approved)
VALUES
  ('startup-conflict-user', 'postgres-conflict-hash', 'admin'::auth_user_role, true),
  ('startup-postgres-only-user', 'postgres-only-hash', 'member'::auth_user_role, false);

INSERT INTO session_states (state_id, user_id, created_at, expires_at, idle_expires_at, revoked_at)
VALUES
  ('11111111-1111-1111-1111-111111111111'::uuid, 'startup-pg-conflict-user', '2025-01-01T00:00:00Z'::timestamptz, '2030-01-01T00:00:00Z'::timestamptz, '2030-01-01T01:00:00Z'::timestamptz, NULL),
  ('22222222-2222-2222-2222-222222222222'::uuid, 'startup-pg-only-user', '2025-01-01T00:00:00Z'::timestamptz, '2030-01-01T00:00:00Z'::timestamptz, '2030-01-01T01:00:00Z'::timestamptz, NULL);

INSERT INTO session_handles (session_id, state_id, issued_at, revoked_at)
VALUES
  ('aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa'::uuid, '11111111-1111-1111-1111-111111111111'::uuid, '2025-01-01T00:00:00Z'::timestamptz, NULL),
  ('bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb'::uuid, '22222222-2222-2222-2222-222222222222'::uuid, '2025-01-01T00:00:00Z'::timestamptz, NULL);

INSERT INTO session_user_bindings (user_id, state_id)
VALUES
  ('startup-binding-conflict-user', '11111111-1111-1111-1111-111111111111'::uuid),
  ('startup-pg-only-user', '22222222-2222-2222-2222-222222222222'::uuid);

INSERT INTO calendar_items (user_id, item_id, item_kind, legacy_item_type, legacy_title, legacy_window_start, legacy_window_end, legacy_status)
VALUES
  ('startup-calendar-conflict-user', 'startup-calendar-conflict-item', 'legacy', 'INTENTION', 'postgres-calendar-conflict-title', '2025-01-01T08:00', '2025-01-01T09:00', 'TODO'),
  ('startup-calendar-postgres-only-user', 'startup-calendar-postgres-only-item', 'legacy', 'INTENTION', 'postgres-calendar-only-title', '2025-01-02T08:00', '2025-01-02T09:00', 'TODO');

INSERT INTO trip_shares (owner_user_id, target_username)
VALUES
  ('startup-trip-sharing-owner', 'startup-trip-sharing-conflict-target'),
  ('startup-trip-sharing-postgres-only-owner', 'startup-trip-sharing-postgres-only-target');

INSERT INTO trip_subscriptions (owner_user_id, target_username)
VALUES
  ('startup-trip-sharing-owner', 'startup-trip-sharing-conflict-target'),
  ('startup-trip-sharing-postgres-only-owner', 'startup-trip-sharing-postgres-only-target');

INSERT INTO note_items (item_id, item_version, item_content)
VALUES
  ('startup-note-conflict-item', 'startup-note-postgres-version', '{"title":"postgres-note-conflict-title","noteContent":"postgres-note-conflict-body"}'::jsonb),
  ('startup-note-postgres-only-item', 'startup-note-postgres-only-version', '{"title":"postgres-note-only-title","noteContent":"postgres-note-only-body"}'::jsonb);

INSERT INTO checklist_items (item_id, item_version, item_content)
VALUES
  ('startup-checklist-conflict-item', 'startup-checklist-postgres-version', '{"name":"postgres-checklist-conflict-name","items":[{"label":"postgres-conflict-item","checked":true}]}'::jsonb),
  ('startup-checklist-postgres-only-item', 'startup-checklist-postgres-only-version', '{"name":"postgres-checklist-only-name","items":[{"label":"postgres-only-item","checked":false}]}'::jsonb);
