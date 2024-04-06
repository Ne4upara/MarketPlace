CREATE TABLE verification_codes (
    id SERIAL PRIMARY KEY,
    user_id INT,
    code VARCHAR(4),
    created_code TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    entry_by_code BOOLEAN DEFAULT TRUE,
    login_attempt INT DEFAULT 0,
    phone_number VARCHAR(13),
    FOREIGN KEY (user_id) REFERENCES users(id)
);