CREATE TABLE products (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    photo VARCHAR(255),
    price DECIMAL(10, 2) NOT NULL,
    category VARCHAR(20) NOT NULL,
    product_type VARCHAR(10) NOT NULL CHECK (product_type IN ('new', 'used')),
    owner_id INT,
    creation_date TIMESTAMP NOT NULL,
    rating INT DEFAULT 0,
    rating_count INT DEFAULT 1,
    description VARCHAR(250) NOT NULL,
    quantity INT NOT NULL,
    FOREIGN KEY (owner_id) REFERENCES users(id)
);