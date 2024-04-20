CREATE TABLE black_list (
    id SERIAL PRIMARY KEY,
    token VARCHAR (200),
    expired_token TIMESTAMP,
    creation_date TIMESTAMP NOT NULL
);

ALTER TABLE black_list ALTER COLUMN creation_date SET DEFAULT now();