CREATE TABLE product_photos (
    id SERIAL PRIMARY KEY,
    product_id BIGINT NOT NULL,
    photo_link VARCHAR(255),
    main_page VARCHAR(255),
    FOREIGN KEY (product_id) REFERENCES products(id)
);
