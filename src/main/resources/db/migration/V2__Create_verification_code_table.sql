CREATE TABLE verification_code (
    id SERIAL PRIMARY KEY,
    code VARCHAR(4),
    created_code TIMESTAMP,
    entry_by_code BOOLEAN NOT NULL DEFAULT TRUE,
    login_attempt INT NOT NULL DEFAULT 0,
    phone_number VARCHAR(13) NOT NULL,
    FOREIGN KEY (phone_number) REFERENCES users(phone_number)
);