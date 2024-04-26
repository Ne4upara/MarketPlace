package ua.marketplace.repositoryes;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import ua.marketplace.entities.Category;

import java.util.Optional;

/**
 * Repository interface for accessing and managing Category entities.
 */
@Repository
public interface CategoryRepository extends JpaRepository<Category, Long> {

    /**
     * Checks if a category with the specified category name exists.
     *
     * @param categoryName The name of the category to check for existence.
     * @return True if a category with the specified name exists, otherwise false.
     */
    Boolean existsByCategoryName(String categoryName);

    /**
     * Retrieves a category by its name.
     *
     * @param categoryName The name of the category to retrieve.
     * @return Optional containing the category with the specified name, or empty if not found.
     */
    Optional<Category> findByCategoryName(String categoryName);

}
