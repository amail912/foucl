CREATE TABLE note_items (
  item_id text NOT NULL,
  item_version text NOT NULL,
  item_content jsonb NOT NULL,
  PRIMARY KEY (item_id),
  CHECK (item_id <> ''),
  CHECK (item_version <> '')
);

CREATE INDEX idx_note_items_item_version ON note_items(item_id, item_version);
