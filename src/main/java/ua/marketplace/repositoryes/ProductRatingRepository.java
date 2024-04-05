package ua.marketplace.repositoryes;

import org.springframework.data.jpa.repository.JpaRepository;
import ua.marketplace.entities.ProductRating;


public interface ProductRatingRepository extends JpaRepository<ProductRating, Long> {
}
