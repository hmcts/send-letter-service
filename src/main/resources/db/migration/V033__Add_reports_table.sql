CREATE TABLE report(
  id UUID,
  report_name VARCHAR(256),
  service VARCHAR(256),
  report_date DATE,
  printed_letters_count INTEGER,
  processed_at TIMESTAMP,
  PRIMARY KEY (id)
);
