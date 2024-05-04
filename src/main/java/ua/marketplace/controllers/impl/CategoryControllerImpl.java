package ua.marketplace.controllers.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import ua.marketplace.controllers.ICategoryController;
import ua.marketplace.entities.Category;
import ua.marketplace.services.impl.CategoryService;

import java.util.List;

/**
 * Controller class for managing categories.
 * Handles requests related to categories.
 */
@RestController
@RequestMapping("/v1/categories")
@RequiredArgsConstructor
public class CategoryControllerImpl implements ICategoryController {

    private final CategoryService categoryService;

    /**
     * Retrieves a list of all categories.
     *
     * @return A list of all categories.
     */
    @Override
    @GetMapping("/list")
    public List<Category> getAllCategory() {
        return categoryService.getAllCategory();
    }
}
