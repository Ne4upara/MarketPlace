package ua.marketplace.services;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.data.ProductCategory;
import ua.marketplace.dto.Pagination;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.User;
import ua.marketplace.repositoryes.ProductRepository;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.ProductRequest;

import java.math.BigDecimal;
import java.security.Principal;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class ProductServiceTest {

    @Mock
    private UserRepository userRepository;
    @Mock
    private ProductRepository productRepository;
    @InjectMocks
    private ProductService productService;


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
        assertEquals(0, result.pageNumber());
        assertEquals(10, result.totalPages());
        verify(productRepository).findAll(any(Pageable.class));
    }

    @Test
    void testGetAllProductsByCategory() {
        // Given
        int pageNumber = 0;
        int pageSize = 10;
        String sortBy = "productName";
        String orderBy = "ASC";
        String category = "dolls";

        // Mocking
        ProductCategory productCategory = ProductCategory.DOLLS;
        Page<Product> mockedPage = mock(Page.class);
        when(productRepository.findByCategory(productCategory,
                PageRequest.of(pageNumber, pageSize, Sort.by(Sort.Direction.fromString(orderBy), sortBy))))
                .thenReturn(mockedPage);
        when(mockedPage.getNumber()).thenReturn(pageNumber);
        when(mockedPage.getTotalElements()).thenReturn(100L);
        when(mockedPage.getTotalPages()).thenReturn(10);

        // When
        Pagination result = productService.getAllProductsByCategory(pageNumber, pageSize, sortBy, orderBy, category);

        // Then
        assertEquals(pageNumber, result.pageNumber());
        assertEquals(10, result.totalPages());
        verify(productRepository).findByCategory(productCategory, PageRequest.of(pageNumber, pageSize, Sort.by(Sort.Direction.fromString(orderBy), sortBy)));
    }

    @Test
    void testGetAllProductsByInvalidCategory() {
        // Given
        int pageNumber = 0;
        int pageSize = 10;
        String sortBy = "productName";
        String orderBy = "ASC";
        String category = "INVALID_CATEGORY"; // Assuming "INVALID_CATEGORY" does not exist

        // When and Then
        assertThrows(ResponseStatusException.class, () ->
                productService.getAllProductsByCategory(pageNumber, pageSize, sortBy, orderBy, category));
    }

    @Test
    void testGetProductDetails() {
        // Given
        long productId = 1L;

        // Mocking
        Product product = new Product();
        product.setId(productId);
        when(productRepository.findById(productId)).thenReturn(Optional.of(product));

        // When
        ProductDto result = productService.getProductDetails(productId);

        // Then
        assertEquals(productId, result.id());
        verify(productRepository).findById(productId);
    }

//    @Test
//    void testSaveProduct() {
//        // Given
//        Principal principal = () -> "user@example.com";
//        ProductRequest request = mockProductRequest();
//
//        // Mocking
//        User user = new User();
//        when(userRepository.findByPhoneNumber(principal.getName())).thenReturn(Optional.of(user));
//        when(productRepository.save(any(Product.class))).thenAnswer(invocation -> invocation.getArgument(0));
//
//        // When
//        ProductDto result = productService.saveProduct(principal, request);
//
//        // Then
//        assertEquals(request.productName(), result.productName());
//        verify(userRepository).findByPhoneNumber(principal.getName());
//        verify(productRepository).save(any(Product.class));
//    }

//    @Test
//    void testUpdateProduct() {
//        // Given
//        Principal principal = () -> "user@example.com";
//        Long productId = 1L;
//        ProductRequest request = mockProductRequest();
//
//        User user = new User();
//        Product product = new Product();
//        product.setOwner(user);
//
//        // Mocking
//        when(userRepository.findByPhoneNumber(principal.getName())).thenReturn(Optional.of(user));
//        when(productRepository.findById(productId)).thenReturn(Optional.of(product));
//        when(productRepository.save(any(Product.class))).thenAnswer(invocation -> invocation.getArgument(0));
//
//        // When
//        ProductDto result = productService.updateProduct(principal, productId, request);
//
//        // Then
//        assertEquals(request.productName(), result.productName());
//        verify(userRepository).findByPhoneNumber(principal.getName());
//        verify(productRepository).findById(productId);
//        verify(productRepository).save(any(Product.class));
//    }

//    @Test
//    void testUpdateProduct_NotOwner() {
//        // Given
//        Principal principal = () -> "user@example.com";
//        Long productId = 1L;
//        ProductRequest request = mockProductRequest();
//
//        User user = new User();
//        Product product = new Product();
//        product.setOwner(new User());
//
//        // Mocking
//        when(userRepository.findByPhoneNumber(principal.getName())).thenReturn(Optional.of(user));
//        when(productRepository.findById(productId)).thenReturn(Optional.of(product));
//
//        // When, Then
//        assertThrows(ResponseStatusException.class, () -> productService.updateProduct(principal, productId, request));
//        verify(userRepository).findByPhoneNumber(principal.getName());
//        verify(productRepository).findById(productId);
//        verify(productRepository, never()).save(any(Product.class));
//    }

//    @Test
//    void testRateProduct() {
//        // Given
//        Principal principal = () -> "user@example.com";
//        Long productId = 1L;
//        int rating = 4;
//
//        User user = new User();
//        Product product = new Product();
//        product.setId(productId);
//        product.setProductRating(3);
//        product.setProductRatingCount(10);
//
//        // Mocking
//        when(userRepository.findByPhoneNumber(principal.getName())).thenReturn(Optional.of(user));
//        when(productRepository.findById(productId)).thenReturn(Optional.of(product));
//        when(productRepository.save(any(Product.class))).thenAnswer(invocation -> invocation.getArgument(0));
//
//        // When
//        ProductDto result = productService.rateProduct(principal, productId, rating);
//
//        // Then
//        assertEquals(0, result.productRating());
//        assertEquals(11, result.productRatingCount());
//        verify(productRepository).findById(productId);
//        verify(productRepository).save(any(Product.class));
//    }

//    @Test
//    void testRateProduct_InvalidRating() {
//        // Given
//        Principal principal = () -> "user@example.com";
//        Long productId = 1L;
//        int rating = 6;
//
//        // When, Then
//        assertThrows(ResponseStatusException.class, () -> productService.rateProduct(principal, productId, rating));
//        verify(productRepository, never()).findById(productId);
//        verify(productRepository, never()).save(any(Product.class));
//    }

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
        verify(userRepository).findByPhoneNumber(principal.getName());
        verify(productRepository).findById(productId);
        verify(productRepository).delete(product);
    }

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
        assertThrows(ResponseStatusException.class, () -> productService.deleteProduct(principal, productId));
        verify(userRepository).findByPhoneNumber(principal.getName());
        verify(productRepository).findById(productId);
        verify(productRepository, never()).delete(any(Product.class));
    }

//    private ProductRequest mockProductRequest() {
//        return new ProductRequest
//                ("Test Product",
//                        "Photo links",
//                        BigDecimal.valueOf(10),
//                        "Test description",
//                        ProductCategory.BOARD_GAMES,
//                        "new",
//                        10);
//    }
}
