package ua.marketplace.services.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import ua.marketplace.entities.Category;
import ua.marketplace.repositoryes.CategoryRepository;
import ua.marketplace.services.ICategoryService;

import java.util.List;

/**
 * Implementation of the {@link ICategoryService} interface for managing categories.
 */
@Service
@RequiredArgsConstructor
public class CategoryService implements ICategoryService {

    private final CategoryRepository categoryRepository;

    /**
     * Retrieves a list of all categories.
     *
     * @return A list of all categories.
     */
    @Override
    public List<Category> getAllCategory() {
        return categoryRepository.findAll();
    }
}
