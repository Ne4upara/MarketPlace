CREATE TABLE products (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    price DECIMAL(10, 2) NOT NULL,
    description VARCHAR(250) NOT NULL,
    category_id INT,
    product_type VARCHAR(10) NOT NULL CHECK (product_type IN ('new', 'used')),
    owner_id INT,
    creation_date TIMESTAMP NOT NULL,
    seller_name VARCHAR(50),
    seller_phone_number VARCHAR(50),
    seller_email VARCHAR (100),
    location VARCHAR (100),
    count_view INT DEFAULT 0,
    relevancy INT,
    is_enabled BOOLEAN DEFAULT TRUE,
    FOREIGN KEY (category_id) REFERENCES product_categories(id),
    FOREIGN KEY (owner_id) REFERENCES users(id)
);