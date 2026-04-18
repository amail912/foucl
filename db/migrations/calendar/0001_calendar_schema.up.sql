CREATE TABLE calendar_items (
  user_id text NOT NULL,
  item_id text NOT NULL,
  item_kind text NOT NULL,

  item_type text,
  title text,
  window_start text,
  window_end text,
  status text,
  source_item_id text,
  actual_duration_minutes integer,
  category text,
  recurrence_rule_type text,
  recurrence_interval_days integer,
  recurrence_exception_dates text[] NOT NULL DEFAULT '{}',

  trip_window_start text,
  trip_window_end text,
  trip_departure_place_id text,
  trip_arrival_place_id text,

  PRIMARY KEY (user_id, item_id),

  CHECK (item_kind IN ('task', 'trip')),
  CHECK (
    item_kind <> 'task' OR (
      item_type IN ('INTENTION', 'BLOC_PLANIFIE')
      AND title IS NOT NULL
      AND window_start IS NOT NULL
      AND window_end IS NOT NULL
      AND status IN ('TODO', 'EN_COURS', 'FAIT', 'ANNULE')
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
      AND item_type IS NULL
      AND title IS NULL
      AND window_start IS NULL
      AND window_end IS NULL
      AND status IS NULL
      AND source_item_id IS NULL
      AND actual_duration_minutes IS NULL
      AND category IS NULL
      AND recurrence_rule_type IS NULL
      AND recurrence_interval_days IS NULL
      AND recurrence_exception_dates = '{}'
    )
  ),
  CHECK (actual_duration_minutes IS NULL OR actual_duration_minutes >= 0),
  CHECK (
    recurrence_rule_type IS NULL
    OR recurrence_rule_type IN ('DAILY', 'WEEKLY', 'MONTHLY', 'YEARLY', 'EVERY_X_DAYS')
  ),
  CHECK (
    recurrence_rule_type <> 'EVERY_X_DAYS'
    OR recurrence_interval_days IS NOT NULL
  ),
  CHECK (
    recurrence_rule_type = 'EVERY_X_DAYS'
    OR recurrence_interval_days IS NULL
  ),
  CHECK (
    recurrence_interval_days IS NULL
    OR recurrence_interval_days > 0
  )
);

CREATE INDEX idx_calendar_items_user_item ON calendar_items(user_id, item_id);
CREATE INDEX idx_calendar_items_user_kind_trip_start ON calendar_items(user_id, item_kind, trip_window_start);
