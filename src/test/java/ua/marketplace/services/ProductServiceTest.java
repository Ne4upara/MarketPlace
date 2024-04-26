package ua.marketplace.services;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.dto.Pagination;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.entities.Category;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.ProductPhoto;
import ua.marketplace.entities.User;
import ua.marketplace.repositoryes.CategoryRepository;
import ua.marketplace.repositoryes.ProductRepository;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.ProductRequest;
import ua.marketplace.utils.ErrorMessageHandler;

// ProductServiceTest class tests the functionality of ProductService
@ExtendWith(MockitoExtension.class)
@SuppressWarnings("PMD")
class ProductServiceTest {

    // UserRepository, ProductRepository, CategoryRepository, and ImageService are mocked
    @Mock
    private UserRepository userRepository;
    @Mock
    private ProductRepository productRepository;
    @Mock
    private CategoryRepository categoryRepository;
    @Mock
    private ImageService imageService;

    // ProductService is created with mocked dependencies
    @InjectMocks
    private ProductService productService;

    // Test method for getting all products for the main page
    @Test
    void testGetAllProductsForMainPage() {
        // Given
        int pageNumber = 0;
        int pageSize = 10;
        String sortBy = "productName";
        String orderBy = "ASC";

        // Mocking
        Page<Product> mockedPage = mock(Page.class);
        when(productRepository.findAll(any(Pageable.class))).thenReturn(mockedPage);
        when(mockedPage.getNumber()).thenReturn(0);
        when(mockedPage.getTotalElements()).thenReturn(100L);
        when(mockedPage.getTotalPages()).thenReturn(10);

        // When
        Pagination result = productService.getAllProductsForMainPage(pageNumber, pageSize, sortBy, orderBy);

        // Then
        // Verify that the correct method from the mocked repository is called
        verify(productRepository).findAll(any(Pageable.class));
    }

    // Test method for getting all products by category
    @Test
    void testGetAllProductsByCategory() {
        // Given
        int pageNumber = 0;
        int pageSize = 10;
        String sortBy = "productName";
        String orderBy = "ASC";
        String categoryName = "Test";

        Product product = mockProduct();

        when(categoryRepository.findByCategoryName(categoryName)).thenReturn(Optional.of(product.getCategory()));

        List<Product> productList = new ArrayList<>();
        productList.add(product);
        productList.add(mockProduct());
        Page<Product> page = new PageImpl<>(productList);
        when(productRepository.findByCategory(any(Category.class), any(Pageable.class))).thenReturn(page);

        // When
        Pagination result = productService.getAllProductsByCategory(pageNumber, pageSize, sortBy, orderBy, categoryName);

        // Then
        // Verify the results and check if the correct methods from the mocked repositories are called
        assertNotNull(result);
        assertEquals(0, result.pageNumber());
        assertEquals(2, result.totalElements());
        assertEquals(1, result.totalPages());
        assertNotNull(result.body());
        assertEquals(2, result.body().size());
        verify(categoryRepository, times(1)).findByCategoryName(categoryName);
    }

    // Test method for getting all products by category with an invalid category
    @Test
    void testGetAllProductsByCategoryWithInvalidCategory() {
        // Given
        int pageNumber = 0;
        int pageSize = 10;
        String sortBy = "productName";
        String orderBy = "ASC";
        String categoryName = "NonExistingCategory";

        when(categoryRepository.findByCategoryName(categoryName)).thenReturn(Optional.empty());

        // When & Then
        // Verify that the correct exception is thrown
        ResponseStatusException exception = assertThrows(ResponseStatusException.class, () -> {
            productService.getAllProductsByCategory(pageNumber, pageSize, sortBy, orderBy, categoryName);
        });

        assertEquals(HttpStatus.NOT_FOUND + " \""
                        + String.format(ErrorMessageHandler.INVALID_CATEGORY, categoryName + "\"")
                , exception.getMessage());
    }

    // Test method for getting product details
    @Test
    void testGetProductDetails() {
        // Given
        long productId = 1L;

        // Mocking
        Product product = mockProduct();
        product.setId(productId);
        when(productRepository.findById(productId)).thenReturn(Optional.of(product));

        // When
        ProductDto result = productService.getProductDetails(productId);

        // Then
        // Verify the results and check if the correct methods from the mocked repositories are called
        assertEquals(productId, result.id());
        verify(productRepository).findById(productId);
    }

    // Test method for saving a product
    @Test
    void testSaveProduct() {
        // Given
        Principal principal = () -> "user@example.com";
        ProductRequest request = mockProductRequest();

        // Mocking
        User user = new User();
        when(userRepository.findByPhoneNumber(principal.getName())).thenReturn(Optional.of(user));
        when(productRepository.save(any(Product.class))).thenAnswer(invocation -> invocation.getArgument(0));
        when(categoryRepository.existsByCategoryName(request.productCategory()))
                .thenReturn(true);
        when(categoryRepository.findByCategoryName(request.productCategory()))
                .thenReturn(Optional.of(new Category(2L, "dolls", "test")));

        // When
        ProductDto result = productService.saveProduct(principal, request);

        // Then
        // Verify the results and check if the correct methods from the mocked repositories are called
        assertEquals(request.productName(), result.productName());
        verify(userRepository).findByPhoneNumber(principal.getName());
        verify(productRepository).save(any(Product.class));
    }

    // Test method for saving a product with an invalid category
    @Test
    void testSaveProductWithInvalidCategory() {
        // Given
        Principal principal = () -> "user@example.com";
        ProductRequest request = mockProductRequest();

        // Mocking
        User user = new User();
        when(userRepository.findByPhoneNumber(principal.getName())).thenReturn(Optional.of(user));
        when(categoryRepository.existsByCategoryName(request.productCategory()))
                .thenReturn(false);

        // When
        ResponseStatusException exception = assertThrows(ResponseStatusException.class, () -> {
            productService.saveProduct(principal, request);
        });

        // Then
        // Verify that the correct exception is thrown
        assertEquals(HttpStatus.CONFLICT + " \""
                        + String.format(ErrorMessageHandler.INVALID_CATEGORY, "dolls" + "\"")
                , exception.getMessage());
    }

    // Test method for updating a product
    @Test
    void testUpdateProduct() {
        // Given
        Principal principal = () -> "user@example.com";
        Long productId = 1L;
        ProductRequest request = mockProductRequest();

        User user = new User();
        Product product = new Product();
        product.setOwner(user);

        // Mocking
        when(userRepository.findByPhoneNumber(principal.getName())).thenReturn(Optional.of(user));
        when(productRepository.findById(productId)).thenReturn(Optional.of(product));
        when(productRepository.save(any(Product.class))).thenAnswer(invocation -> invocation.getArgument(0));
        when(categoryRepository.existsByCategoryName(request.productCategory()))
                .thenReturn(true);
        when(categoryRepository.findByCategoryName(request.productCategory()))
                .thenReturn(Optional.of(new Category(2L, "dolls", "test")));

        // When
        ProductDto result = productService.updateProduct(principal, productId, request);

        // Then
        // Verify the results and check if the correct methods from the mocked repositories are called
        assertEquals(request.productName(), result.productName());
        verify(userRepository).findByPhoneNumber(principal.getName());
        verify(productRepository).findById(productId);
        verify(productRepository).save(any(Product.class));
    }

    // Test method for updating a product with a different owner
    @Test
    void testUpdateProductWithNotOwner() {
        // Given
        Principal principal = () -> "user@example.com";
        Long productId = 1L;
        ProductRequest request = mockProductRequest();

        User user = new User();
        Product product = new Product();
        product.setOwner(new User());

        // Mocking
        when(userRepository.findByPhoneNumber(principal.getName())).thenReturn(Optional.of(user));
        when(productRepository.findById(productId)).thenReturn(Optional.of(product));

        // When, Then
        // Verify that the correct exception is thrown
        assertThrows(ResponseStatusException.class, () -> productService.updateProduct(principal, productId, request));
        verify(userRepository).findByPhoneNumber(principal.getName());
        verify(productRepository).findById(productId);
        verify(productRepository, never()).save(any(Product.class));
    }

    // Test method for deleting a product
    @Test
    void testDeleteProduct() {
        // Given
        Principal principal = () -> "user@example.com";
        Long productId = 1L;

        User user = new User();
        Product product = new Product();
        product.setOwner(user);

        // Mocking
        when(userRepository.findByPhoneNumber(principal.getName())).thenReturn(Optional.of(user));
        when(productRepository.findById(productId)).thenReturn(Optional.of(product));

        // When
        productService.deleteProduct(principal, productId);

        // Then
        // Verify the results and check if the correct methods from the mocked repositories are called
        verify(userRepository).findByPhoneNumber(principal.getName());
        verify(productRepository).findById(productId);
        verify(productRepository).delete(product);
    }

    // Test method for deleting a product with a different owner
    @Test
    void testDeleteProduct_NotOwner() {
        // Given
        Principal principal = () -> "user@example.com";
        Long productId = 1L;

        User user = new User();
        Product product = new Product();
        product.setOwner(new User());

        // Mocking
        when(userRepository.findByPhoneNumber(principal.getName())).thenReturn(Optional.of(user));
        when(productRepository.findById(productId)).thenReturn(Optional.of(product));

        // When, Then
        // Verify that the correct exception is thrown
        assertThrows(ResponseStatusException.class, () -> productService.deleteProduct(principal, productId));
        verify(userRepository).findByPhoneNumber(principal.getName());
        verify(productRepository).findById(productId);
        verify(productRepository, never()).delete(any(Product.class));
    }

    // Helper method for creating a mock Product
    private Product mockProduct() {
        List<ProductPhoto> photo = new ArrayList<>();
        Category category = new Category(1L, "Test", "ТЕСТ");
        return Product
                .builder()
                .productName("test")
                .photos(photo)
                .productPrice(BigDecimal.valueOf(10))
                .productDescription("test description")
                .category(category)
                .productType("new")
                .owner(null)
                .build();
    }

    // Helper method for creating a mock ProductRequest
    private ProductRequest mockProductRequest() {
        return new ProductRequest
                ("Test Product",
                        new ArrayList<>(),
                        BigDecimal.valueOf(10),
                        "test description",
                        "dolls",
                        "new",
                        "Seller",
                        "+380999999999",
                        "seller@mail.com",
                        "Location"
                );
    }
}
