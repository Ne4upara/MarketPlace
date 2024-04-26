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
import ua.marketplace.entities.User;

/**
 * A repository interface for accessing product data in the database.
 */
@Repository
public interface ProductRepository extends JpaRepository<Product, Long> {

    /**
     * Retrieves a page of products belonging to the specified category.
     *
     * @param category The category to filter products by.
     * @param pageable The pagination information.
     * @return A page of products belonging to the specified category.
     */
    Page<Product> findByCategory(Category category, Pageable pageable);

    /**
     * Increments the view count of a product by one.
     *
     * @param id The ID of the product whose view count needs to be incremented.
     */
    @Transactional
    @Modifying
    @Query("UPDATE Product p SET p.countView = p.countView + 1 WHERE p.id = :id")
    void incrementProductViews(@Param("id") Long id);

    Page<Product> findAllByOwner(User user, Pageable pageable);

}
