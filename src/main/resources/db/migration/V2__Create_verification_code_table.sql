CREATE TABLE verification_codes (
    id SERIAL PRIMARY KEY,
    user_id INT,
    code VARCHAR(4),
    created_code TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    entry_by_code BOOLEAN DEFAULT TRUE,
    login_attempt INT DEFAULT 0,
    FOREIGN KEY (user_id) REFERENCES users(id)
);

INSERT INTO verification_codes (user_id, code) VALUES
    ('1', '1111'),
    ('2', '1111');
