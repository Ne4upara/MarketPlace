CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    phone_number VARCHAR(13) UNIQUE NOT NULL,
    first_name VARCHAR(15) NOT NULL,
    role VARCHAR(20) NOT NULL DEFAULT 'USER',
    is_enabled BOOLEAN DEFAULT TRUE,
    creation_date TIMESTAMP NOT NULL
);

CREATE INDEX idx_phone_number ON users (phone_number);

ALTER TABLE users ALTER COLUMN creation_date SET DEFAULT now();

INSERT INTO users (phone_number, first_name, role) VALUES
    ('+380123456789', 'Админ', 'ADMIN'),
    ('+380999999999','Админ','ADMIN');
