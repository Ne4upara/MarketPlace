CREATE TABLE buckets (
    id SERIAL PRIMARY KEY,
    user_id BIGINT,
    total_price NUMERIC,
    FOREIGN KEY (user_id) REFERENCES users(id)
);

CREATE TABLE product_bucket (
    product_bucket_id BIGINT,
    product_id BIGINT,
    FOREIGN KEY (product_bucket_id) REFERENCES buckets(id),
    FOREIGN KEY (product_id) REFERENCES products(id)
);
