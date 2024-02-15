CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  phone_number VARCHAR(20) NOT NULL,
  sms_code INT,
  sms_code_create_at TIMESTAMP
);