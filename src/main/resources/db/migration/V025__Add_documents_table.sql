CREATE TABLE IF NOT EXISTS documents
(
  id          BIGSERIAL    PRIMARY KEY,
  letter_id   UUID         NOT NULL REFERENCES letters ON DELETE CASCADE,
  created_at  TIMESTAMP    NOT NULL,
  checksum    VARCHAR(256) NULL
);
