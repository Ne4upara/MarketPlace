package ua.marketplace.repositoryes;

import org.springframework.data.jpa.repository.JpaRepository;
import ua.marketplace.entities.ProductRating;

/**
 * Repository interface for accessing and managing ProductRating entities.
 */
//TODO
public interface ProductRatingRepository extends JpaRepository<ProductRating, Long> {

}
