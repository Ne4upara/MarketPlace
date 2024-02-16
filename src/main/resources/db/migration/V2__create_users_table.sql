CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  first_name VARCHAR(255) NOT NULL,
  phone_number VARCHAR(20) NOT NULL,
  sms_code_id BIGINT,
  CONSTRAINT fk_sms_code_id FOREIGN KEY (sms_code_id) REFERENCES sms_codes(id)
);
