CREATE TABLE calendar_items (
  user_id text NOT NULL,
  item_id text NOT NULL,
  item_kind text NOT NULL,

  legacy_item_type text,
  legacy_title text,
  legacy_window_start text,
  legacy_window_end text,
  legacy_status text,
  legacy_source_item_id text,
  legacy_actual_duration_minutes integer,
  legacy_category text,
  legacy_recurrence_rule_type text,
  legacy_recurrence_interval_days integer,
  legacy_recurrence_exception_dates text[] NOT NULL DEFAULT '{}',

  trip_window_start text,
  trip_window_end text,
  trip_departure_place_id text,
  trip_arrival_place_id text,

  PRIMARY KEY (user_id, item_id),

  CHECK (item_kind IN ('legacy', 'trip')),
  CHECK (
    item_kind <> 'legacy' OR (
      legacy_item_type IN ('INTENTION', 'BLOC_PLANIFIE')
      AND legacy_title IS NOT NULL
      AND legacy_window_start IS NOT NULL
      AND legacy_window_end IS NOT NULL
      AND legacy_status IN ('TODO', 'EN_COURS', 'FAIT', 'ANNULE')
      AND trip_window_start IS NULL
      AND trip_window_end IS NULL
      AND trip_departure_place_id IS NULL
      AND trip_arrival_place_id IS NULL
    )
  ),
  CHECK (
    item_kind <> 'trip' OR (
      trip_window_start IS NOT NULL
      AND trip_window_end IS NOT NULL
      AND trip_departure_place_id IS NOT NULL
      AND trip_arrival_place_id IS NOT NULL
      AND legacy_item_type IS NULL
      AND legacy_title IS NULL
      AND legacy_window_start IS NULL
      AND legacy_window_end IS NULL
      AND legacy_status IS NULL
      AND legacy_source_item_id IS NULL
      AND legacy_actual_duration_minutes IS NULL
      AND legacy_category IS NULL
      AND legacy_recurrence_rule_type IS NULL
      AND legacy_recurrence_interval_days IS NULL
      AND legacy_recurrence_exception_dates = '{}'
    )
  ),
  CHECK (legacy_actual_duration_minutes IS NULL OR legacy_actual_duration_minutes >= 0),
  CHECK (
    legacy_recurrence_rule_type IS NULL
    OR legacy_recurrence_rule_type IN ('DAILY', 'WEEKLY', 'MONTHLY', 'YEARLY', 'EVERY_X_DAYS')
  ),
  CHECK (
    legacy_recurrence_rule_type <> 'EVERY_X_DAYS'
    OR legacy_recurrence_interval_days IS NOT NULL
  ),
  CHECK (
    legacy_recurrence_rule_type = 'EVERY_X_DAYS'
    OR legacy_recurrence_interval_days IS NULL
  ),
  CHECK (
    legacy_recurrence_interval_days IS NULL
    OR legacy_recurrence_interval_days > 0
  )
);

CREATE INDEX idx_calendar_items_user_item ON calendar_items(user_id, item_id);
CREATE INDEX idx_calendar_items_user_kind_trip_start ON calendar_items(user_id, item_kind, trip_window_start);
