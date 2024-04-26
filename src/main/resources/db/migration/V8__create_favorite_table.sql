CREATE TABLE favorites(
    id SERIAL PRIMARY KEY,
    user_id INT,
    product_id INT,
    creation_date TIMESTAMP NOT NULL,
    FOREIGN KEY (user_id) REFERENCES users(id),
    FOREIGN KEY (product_id) REFERENCES products(id)
);