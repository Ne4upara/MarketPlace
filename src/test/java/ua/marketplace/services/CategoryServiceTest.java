package ua.marketplace.services;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import ua.marketplace.entities.Category;
import ua.marketplace.repositoryes.CategoryRepository;
import ua.marketplace.services.impl.CategoryServiceImpl;


import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@SuppressWarnings("PMD")
class CategoryServiceTest {

    @Mock
    private CategoryRepository categoryRepository;
    @InjectMocks
    private CategoryServiceImpl categoryService;

    @Test
    void testGetAllCategory() {

        //Given
        List<Category> expect = new ArrayList<>();
        Category category1 = new Category(1L, "Test1", "Test1");
        Category category2 = new Category(2L, "Test2", "Test2");
        expect.add(category1);
        expect.add(category2);

        when(categoryRepository.findAll()).thenReturn(expect);

        //When
        List<Category> result = categoryService.getAllCategories();

        //Then
        assertEquals(expect, result);
    }
}
