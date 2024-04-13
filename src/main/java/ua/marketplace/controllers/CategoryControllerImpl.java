package ua.marketplace.controllers;

import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import ua.marketplace.entities.Category;
import ua.marketplace.services.CategoryService;

import java.util.List;

@RestController
@RequestMapping("/v1/categories")
@RequiredArgsConstructor
public class CategoryControllerImpl implements ICategoryController {

    private final CategoryService categoryService;

    @Override
    @GetMapping("/list")
    public List<Category> getAllCategory() {
        return categoryService.getAllCategory();
    }
}
