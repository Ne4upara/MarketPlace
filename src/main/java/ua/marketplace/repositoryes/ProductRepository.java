package ua.marketplace.repositoryes;

import jakarta.transaction.Transactional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import ua.marketplace.entities.Category;
import ua.marketplace.entities.Product;


/**
 * A repository interface for accessing product data in the database.
 */
@Repository
public interface ProductRepository extends JpaRepository<Product, Long> {
    Page<Product> findByCategory(Category category, Pageable pageable);

    @Transactional
    @Modifying
    @Query("UPDATE Product p SET p.countView = p.countView + 1 WHERE p.id = :id")
    void incrementProductViews(@Param("id") Long id);
}
