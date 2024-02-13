CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  phone_number VARCHAR(13) NOT NULL,
  code INT
);