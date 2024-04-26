package ua.marketplace.repositoryes;

import org.springframework.data.jpa.repository.JpaRepository;
import ua.marketplace.entities.Favorite;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.User;

public interface FavoriteRepository extends JpaRepository<Favorite, Long> {

    Favorite findByUserAndProduct(User user, Product product);
}
