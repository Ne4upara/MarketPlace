package ua.marketplace.repositoryes;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import ua.marketplace.entities.Favorite;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.User;

import java.util.Set;

/**
 * Repository interface for accessing and managing Favorite entities in the database.
 * Extends JpaRepository for basic CRUD operations.
 */
public interface FavoriteRepository extends JpaRepository<Favorite, Long> {

    /**
     * Retrieves a favorite association by the associated user and product.
     *
     * @param user The user associated with the favorite.
     * @param product The product associated with the favorite.
     * @return The favorite association, if found, otherwise null.
     */
    Favorite findByUserAndProduct(User user, Product product);

    /**
     * Checks if a favorite association exists for the given user and product.
     *
     * @param user The user associated with the favorite.
     * @param product The product associated with the favorite.
     * @return True if a favorite association exists, otherwise false.
     */
    Boolean existsByUserAndProduct(User user, Product product);

    /**
     * Retrieves all favorite associations for a given user, paginated.
     *
     * @param user The user whose favorite associations are to be retrieved.
     * @param pageable The pagination information.
     * @return A page containing favorite associations for the user.
     */
    Page<Favorite> findAllByUser(User user, Pageable pageable);

    /**
     * Retrieves all favorite associations for a given user.
     *
     * @param user The user whose favorite associations are to be retrieved.
     * @return A set containing all favorite associations for the user.
     */
    Set<Favorite> findByUser(User user);
}
