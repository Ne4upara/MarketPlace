package ua.marketplace.repositoryes;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import ua.marketplace.entities.Product;

/**
 * A repository interface for accessing product data in the database.
 */
@Repository
public interface ProductRepository extends JpaRepository<Product, Long> {
}
