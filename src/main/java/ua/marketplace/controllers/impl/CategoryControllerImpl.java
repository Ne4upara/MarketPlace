package ua.marketplace.controllers.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import ua.marketplace.controllers.CategoryController;
import ua.marketplace.entities.Category;
import ua.marketplace.services.impl.CategoryServiceImpl;

import java.util.List;

/**
 * Controller class for managing categories.
 * Handles requests related to categories.
 */
@RestController
@RequestMapping("/v1/categories")
@RequiredArgsConstructor
public class CategoryControllerImpl implements CategoryController {

    private final CategoryServiceImpl categoryService;

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
