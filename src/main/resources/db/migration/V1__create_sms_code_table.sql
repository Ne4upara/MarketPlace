CREATE TABLE sms_codes (
  id SERIAL PRIMARY KEY,
  code VARCHAR(255),
  create_at TIMESTAMP,
  is_enable BOOLEAN NOT NULL DEFAULT TRUE,
  verification_attempts INT DEFAULT 3
);
