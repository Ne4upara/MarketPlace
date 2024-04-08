package ua.marketplace.repositoryes;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import ua.marketplace.entities.Category;

@Repository
public interface CategoryRepository extends JpaRepository<Category, Long> {

    Boolean existsByCategoryName(String categoryName);

    Category findByCategoryName(String categoryName);
}
