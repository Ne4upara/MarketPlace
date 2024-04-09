CREATE TABLE product_categories (
    id SERIAL PRIMARY KEY,
    name_url VARCHAR(255) NOT NULL,
    name_ukr VARCHAR(255) NOT NULL
);

INSERT INTO product_categories (name_url, name_ukr) VALUES
    ('for_infants', 'ДЛЯ НЕМОВЛЯТ'),
    ('dolls', 'ЛЯЛЬКИ, ПУПСИ'),
    ('plush_toy', 'М*ЯКА ІГРАШКА'),
-- апостроф це специфічний символ який не підтримується в SQL :(

    ('transformers', 'ТРАНСФОРМЕРИ'),
    ('constructors', 'КОНСТРУКТОРИ'),
    ('puzzles', 'ПАЗЛИ'),
    ('toy_cars', 'МАШИНКИ'),
    ('collectible_car_models', 'КОЛЕКЦІЙНІ МОДЕЛІ МАШИН'),
    ('toy_car_tracks', 'АВТОМОБІЛЬНІ ТРЕКИ'),
    ('train_sets', 'ЗАЛІЗНИЧНІ НАБОРИ'),
    ('remote_control_toys', 'РАДІОКЕРОВАНІ ІГРАШКИ'),
    ('airplanes_helicopters', 'ЛІТАКИ, ГЕЛІКОПТЕРИ'),
    ('craft_kits', 'НАБОРИ ДЛЯ ТВОРЧОСТІ'),
    ('board_games', 'НАСТІЛЬНІ ІГРИ'),
    ('childrens_books', 'ДИТЯЧІ КНИЖКИ'),
    ('childrens_furniture', 'ДИТЯЧІ МЕБЛІ'),
    ('childrens_transport', 'ДИТЯЧИЙ ТРАНСПОРТ');