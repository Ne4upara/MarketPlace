package ua.marketplace.repositoryes;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import ua.marketplace.data.ProductCategory;
import ua.marketplace.entities.Product;


/**
 * A repository interface for accessing product data in the database.
 */
@Repository
public interface ProductRepository extends JpaRepository<Product, Long> {
    Page<Product> findByProductCategory(ProductCategory category, Pageable pageable);
}
