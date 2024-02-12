CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  phone VARCHAR(13) NOT NULL,
  password VARCHAR(20) NOT NULL,
  code VARCHAR(6),
  created_code TIMESTAMP NOT NULL,
  role VARCHAR(20) NOT NULL,
  is_enabled BOOLEAN NOT NULL
);