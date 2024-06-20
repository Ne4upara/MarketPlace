package ua.marketplace.services;

import ua.marketplace.entities.Category;

import java.util.List;

/**
 * Service interface for managing categories.
 */
public interface CategoryService {

    /**
     * Retrieves a list of all categories.
     *
     * @return A list of all categories.
     */
    List<Category> getAllCategory();
}
