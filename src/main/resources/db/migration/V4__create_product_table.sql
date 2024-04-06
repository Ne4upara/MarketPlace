CREATE TABLE products (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    price DECIMAL(10, 2) NOT NULL,
--     возможно текст, и может быть пустое? дескриптион
    description VARCHAR(250) NOT NULL,
    category_id INT,
    product_type VARCHAR(10) NOT NULL CHECK (product_type IN ('new', 'used')),
    owner_id INT,
    quantity INT NOT NULL,
    creation_date TIMESTAMP NOT NULL,
    FOREIGN KEY (category_id) REFERENCES product_categories(id),
    FOREIGN KEY (owner_id) REFERENCES users(id)
);