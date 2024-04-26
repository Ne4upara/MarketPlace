CREATE TABLE product_categories (
    id SERIAL PRIMARY KEY,
    name_url VARCHAR(255) NOT NULL,
    name_ukr VARCHAR(255) NOT NULL
);

INSERT INTO product_categories (name_url, name_ukr) VALUES
    ('for_infants', 'Для Немовлят'),
    ('dolls', 'Ляльки, Пупси'),
    ('plush_toy', 'М`яка іграшка'),
    ('transformers', 'Трансформери'),
    ('constructors', 'Конструктори'),
    ('puzzles', 'Пазли'),
    ('toy_cars', 'Машинки'),
    ('collectible_car_models', 'Колекційні моделі машин'),
    ('toy_car_tracks', 'Автомобільні треки'),
    ('train_sets', 'Залізничні набори'),
    ('remote_control_toys', 'Радіокеровані іграшки'),
    ('airplanes_helicopters', 'Літаки, Гелікоптери'),
    ('craft_kits', 'Набори для творчості'),
    ('board_games', 'Настільні ігри'),
    ('childrens_books', 'Дитячі книжки'),
    ('childrens_furniture', 'Дитячи меблі'),
    ('childrens_transport', 'Дитячий транспорт');
