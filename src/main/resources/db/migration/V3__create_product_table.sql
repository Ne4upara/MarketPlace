CREATE TABLE products (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    photo VARCHAR(255),
    price DECIMAL(10, 2) NOT NULL,
    category VARCHAR(100) ,
    product_type ENUM('new', 'used') NOT NULL,
    owner_id INT,
    creation_date DATETIME NOT NULL,
    rating INT DEFAULT 0,
    rating_count INT DEFAULT 1,
    description TEXT NOT NULL,
    FOREIGN KEY (owner_id) REFERENCES users(id)
);