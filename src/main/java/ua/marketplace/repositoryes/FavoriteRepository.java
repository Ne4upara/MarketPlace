package ua.marketplace.repositoryes;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import ua.marketplace.entities.Favorite;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.User;

import java.util.Set;

public interface FavoriteRepository extends JpaRepository<Favorite, Long> {

    Favorite findByUserAndProduct(User user, Product product);

    Boolean existsByUserAndProduct(User user, Product product);

    Page<Favorite> findAllByUser(User user, Pageable pageable);

    Set<Favorite> findByUser(User user);

}
