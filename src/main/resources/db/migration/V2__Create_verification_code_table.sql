CREATE TABLE verification_codes (
    id SERIAL PRIMARY KEY,
    code VARCHAR(4),
    created_code TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    entry_by_code BOOLEAN DEFAULT TRUE,
    login_attempt INT DEFAULT 0,
    phone_number VARCHAR(13),
    FOREIGN KEY (phone_number) REFERENCES users(phone_number)
);