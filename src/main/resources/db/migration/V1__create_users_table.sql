CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    phone_number VARCHAR(13) UNIQUE NOT NULL,
    first_name VARCHAR(15) NOT NULL,
    role VARCHAR(20) NOT NULL,
    is_enabled BOOLEAN DEFAULT TRUE
);

CREATE INDEX idx_phone_number ON users (phone_number);