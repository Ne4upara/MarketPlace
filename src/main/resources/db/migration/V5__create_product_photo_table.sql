CREATE TABLE product_photos (
    id SERIAL PRIMARY KEY,
    product_id BIGINT NOT NULL,
    photo_link VARCHAR(255),
    main_page BOOLEAN,
    creation_date TIMESTAMP NOT NULL,
    FOREIGN KEY (product_id) REFERENCES products(id)
);
