UPDATE letters
SET checksum = message_id
WHERE checksum IS NULL;
