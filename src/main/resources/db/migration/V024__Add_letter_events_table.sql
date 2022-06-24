CREATE TABLE letter_events
(
    id          BIGSERIAL    PRIMARY KEY,
    letter_id   UUID         NOT NULL REFERENCES letters ON DELETE CASCADE,
    created_at  TIMESTAMP    NOT NULL,
    type        VARCHAR(100) NOT NULL,
    notes       TEXT         NULL
);
