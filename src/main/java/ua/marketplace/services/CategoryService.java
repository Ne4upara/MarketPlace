package ua.marketplace.services;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import ua.marketplace.entities.Category;
import ua.marketplace.repositoryes.CategoryRepository;

import java.util.List;

@Service
@RequiredArgsConstructor
public class CategoryService implements ICategoryService {

    private final CategoryRepository categoryRepository;

    @Override
    public List<Category> getAllCategory() {
        return categoryRepository.findAll();
    }
}
