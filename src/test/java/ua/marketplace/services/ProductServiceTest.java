package ua.marketplace.services;

import jakarta.servlet.http.HttpSession;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.*;
import org.springframework.http.HttpStatus;
import org.springframework.test.annotation.Rollback;
import org.springframework.test.context.TestPropertySource;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.dto.Pagination;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.entities.*;
import ua.marketplace.repositoryes.CategoryRepository;
import ua.marketplace.repositoryes.FavoriteRepository;
import ua.marketplace.repositoryes.ProductRepository;
import ua.marketplace.requests.ProductRequest;
import ua.marketplace.services.impl.ImageService;
import ua.marketplace.services.impl.ProductService;
import ua.marketplace.services.impl.UtilsService;
import ua.marketplace.utils.ErrorMessageHandler;

import java.math.BigDecimal;
import java.security.Principal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@TestPropertySource(locations="classpath:application-dev.properties")
@Transactional
@SuppressWarnings("PMD")
class ProductServiceTest {

    @Mock
    private ProductRepository productRepository;
    @Mock
    private CategoryRepository categoryRepository;
    @Mock
    private UtilsService utilsService;
    @Mock
    private ImageService imageService;
    @Mock
    private FavoriteRepository favoriteRepository;
    @InjectMocks
    private ProductService productService;


    @Test
    @Rollback
    void testGetAllProductsForMainPage() {
        // Given
        int pageNumber = 0;
        int pageSize = 10;
        String sortBy = "productName";
        String orderBy = "ASC";

        // Mocking
        Page<Product> mockedPage = mock(Page.class);
        PageRequest pageRequest = PageRequest.of(pageNumber, pageSize, Sort.by(sortBy).ascending());
        when(utilsService.getPageRequest(pageNumber, pageSize, sortBy, orderBy)).thenReturn(pageRequest);
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
    @Rollback
    void testGetAllProductsByCategory() {
        // Given
        int pageNumber = 0;
        int pageSize = 10;
        String sortBy = "productName";
        String orderBy = "ASC";
        String categoryName = "dolls";

        Product product = mockProduct();

        when(categoryRepository.findByCategoryName(categoryName)).thenReturn(Optional.of(product.getCategory()));

        List<Product> productList = new ArrayList<>();
        productList.add(product);
        productList.add(mockProduct());
        Page<Product> page = new PageImpl<>(productList);
        PageRequest pageRequest = PageRequest.of(pageNumber, pageSize, Sort.by(sortBy).ascending());
        when(utilsService.getPageRequest(pageNumber, pageSize, sortBy, orderBy)).thenReturn(pageRequest);
        when(productRepository.findByCategory(any(Category.class), any(Pageable.class))).thenReturn(page);

        // When
        Pagination result = productService.getAllProductsByCategory(pageNumber, pageSize, sortBy, orderBy, categoryName);

        // Then
        assertNotNull(result);
        assertEquals(0, result.pageNumber());
        assertEquals(2, result.totalElements());
        assertEquals(1, result.totalPages());
        assertNotNull(result.body());
        assertEquals(0, result.body().size());
        verify(categoryRepository, times(1)).findByCategoryName(categoryName);
    }

    @Test
    @Rollback
    void testGetAllProductsByCategoryWithInvalidCategory() {
        // Given
        int pageNumber = 0;
        int pageSize = 10;
        String sortBy = "productName";
        String orderBy = "ASC";
        String categoryName = "NonExistingCategory";

        when(categoryRepository.findByCategoryName(categoryName)).thenReturn(Optional.empty());

        // When & Then
        ResponseStatusException exception = assertThrows(ResponseStatusException.class, () -> {
            productService.getAllProductsByCategory(pageNumber, pageSize, sortBy, orderBy, categoryName);
        });

        assertEquals(HttpStatus.NOT_FOUND + " \""
                        + String.format(ErrorMessageHandler.INVALID_CATEGORY, categoryName + "\"")
                , exception.getMessage());
    }

    @Test
    @Rollback
    void testGetProductDetails() {
        // Given
        long productId = 1L;

        // Mocking
        Product product = mockProduct();
        product.setId(productId);
        HttpSession session;
        when(utilsService.getProductById(productId)).thenReturn(product);

        // When
        ProductDto result = productService.getProductDetails(productId);

        // Then
        assertEquals(productId, result.id());
        verify(utilsService).getProductById(productId);
    }

//    @Test
//    void testSaveProduct() {
//        // Given
//        Principal principal = () -> "user@example.com";
//        ProductRequest request = mockProductRequest();
//
//        // Mocking
//        User user = new User();
//        when(utilsService.getUserByPrincipal(principal)).thenReturn(user);
//        when(productRepository.save(any(Product.class))).thenAnswer(invocation -> invocation.getArgument(0));
//        when(categoryRepository.existsByCategoryName(request.productCategory()))
//                .thenReturn(true);
//        when(categoryRepository.findByCategoryName(request.productCategory()))
//                .thenReturn(Optional.of(new Category(2L, "dolls", "test")));
//
//        // When
//        ProductDto result = productService.saveProduct(principal, request);
//
//        // Then
//        assertEquals(request.productName(), result.productName());
//        verify(productRepository).save(any(Product.class));
//    }

//    @Test
//    void testSaveProductWithInvalidCategory() {
//        // Given
//        Principal principal = () -> "user@example.com";
//        ProductRequest request = mockProductRequest();
//
//        // Mocking
//        User user = new User();
//        when(utilsService.getUserByPrincipal(principal)).thenReturn(user);
//        when(categoryRepository.existsByCategoryName(request.productCategory()))
//                .thenReturn(false);
//
//        // When
//        ResponseStatusException exception = assertThrows(ResponseStatusException.class, () -> {
//            productService.saveProduct(principal, request);
//        });
//
//        // Then
//        assertEquals(HttpStatus.CONFLICT + " \""
//                        + String.format(ErrorMessageHandler.INVALID_CATEGORY, "dolls" + "\"")
//                , exception.getMessage());
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
//        when(utilsService.getUserByPrincipal(principal)).thenReturn(user);
//        when(utilsService.getProductById(productId)).thenReturn(product);
//        when(productRepository.save(any(Product.class))).thenAnswer(invocation -> invocation.getArgument(0));
//        when(categoryRepository.existsByCategoryName(request.productCategory()))
//                .thenReturn(true);
//        when(categoryRepository.findByCategoryName(request.productCategory()))
//                .thenReturn(Optional.of(new Category(2L, "dolls", "test")));
//
//        // When
//        ProductDto result = productService.updateProduct(principal, productId, request);
//
//        // Then
//        assertEquals(request.productName(), result.productName());
//        verify(utilsService).getProductById(productId);
//        verify(productRepository).save(any(Product.class));
//    }

//    @Test
//    void testUpdateProductWithNotOwner() {
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
//        when(utilsService.getUserByPrincipal(principal)).thenReturn(user);
//        when(utilsService.getProductById(productId)).thenReturn(product);
//
//        // When, Then
//        assertThrows(ResponseStatusException.class, () -> productService.updateProduct(principal, productId, request));
//        verify(utilsService).getProductById(productId);
//        verify(productRepository, never()).save(any(Product.class));
//    }

    @Test
    @Rollback
    void testDeleteProduct() {
        // Given
        Principal principal = () -> "userexamplecom";
        Long productId = 1L;

        User user = new User();
        Product product = new Product();
        product.setOwner(user);

        // Mocking
        when(utilsService.getUserByPrincipal(principal)).thenReturn(user);
        when(utilsService.getProductById(productId)).thenReturn(product);

        // When
        productService.deleteProduct(principal, productId);

        // Then
        verify(utilsService).getProductById(productId);
        verify(productRepository).delete(product);
    }

    @Test
    @Rollback
    void testDeleteProduct_NotOwner() {
        // Given
        Principal principal = () -> "userexample";
        Long productId = 1L;

        User user = new User();
        Product product = new Product();
        product.setOwner(new User());

        // Mocking
        when(utilsService.getUserByPrincipal(principal)).thenReturn(user);
        when(utilsService.getProductById(productId)).thenReturn(product);

        // When, Then
        assertThrows(ResponseStatusException.class, () -> productService.deleteProduct(principal, productId));
        verify(utilsService).getProductById(productId);
        verify(productRepository, never()).delete(any(Product.class));
    }

    @Test
    @Rollback
    void testGetFavorite_AddFavoriteSuccessfully() {
        // Given
        Principal principal = () -> "user@example.com";
        Product product = new Product();

        // Mocking
        when(utilsService.getUserByPrincipal(principal)).thenReturn(new User());
        when(favoriteRepository.existsByUserAndProduct(any(User.class), any(Product.class))).thenReturn(false);
        when(utilsService.getProductById(any())).thenReturn(product);

        // When
        productService.getFavorite(principal, product.getId());

        // Then
        verify(favoriteRepository, times(1)).save(any(Favorite.class));
    }

    @Test
    @Rollback
    void testGetFavorite_AddDuplicateFavorite() {
        // Given
        Principal principal = () -> "user@example.com";
        Long productId = 123L;

        User user = new User();
        Product product = new Product();
        product.setId(productId);

        // Mocking
        when(utilsService.getUserByPrincipal(principal)).thenReturn(user);
        when(utilsService.getProductById(productId)).thenReturn(product);
        when(favoriteRepository.existsByUserAndProduct(user, product)).thenReturn(true);

        // When, Then
        assertThrows(ResponseStatusException.class, () -> productService.getFavorite(principal, productId));
    }

    @Test
    @Rollback
    void testDeleteFavorite_RemoveFavoriteSuccessfully() {

        // Given
        Principal principal = () -> "user@example.com";
        User user = new User();
        Product product = new Product();
        Favorite favorite = new Favorite();
        List<Favorite> favorites = new ArrayList<>();
        favorites.add(favorite);
        favorite.setProduct(product);
        favorite.setUser(user);
        user.setFavorites(favorites);
        product.setFavorites(favorites);

        // Mocking
        when(utilsService.getProductById(any())).thenReturn(product);
        when(favoriteRepository.existsByUserAndProduct(any(User.class), any(Product.class)))
                .thenReturn(true);
        when(utilsService.getUserByPrincipal(principal)).thenReturn(user);
        when(favoriteRepository.findByUserAndProduct(any(User.class), any(Product.class)))
                .thenReturn(favorite);

        // When
        productService.deleteFavorite(principal, product.getId());

        // Then
        verify(favoriteRepository, times(1)).delete(any(Favorite.class));
    }

    @Test
    @Rollback
    void testDeleteFavorite_RemoveNonExistingFavorite() {

        //Given
        Principal principal = () -> "user@example.com";
        Product product = new Product();

        // Mock
        when(utilsService.getUserByPrincipal(principal)).thenReturn(new User());
        when(utilsService.getProductById(product.getId())).thenReturn(product);
        lenient().when(favoriteRepository.findByUserAndProduct(any(User.class), any(Product.class))).thenReturn(null);

        // When, Then
        assertThrows(ResponseStatusException.class, () -> productService.deleteFavorite(principal, product.getId()));
    }

    @Test
    @Rollback
    void testGetFavorite_InvalidFavorite() {

        // Given
        Principal principal = () -> "user@example.com";
        Product product = new Product();
        User user = new User();
        user.setId(1L);

        // Mocking
        when(utilsService.getUserByPrincipal(principal)).thenReturn(user);
        when(utilsService.getProductById(product.getId())).thenReturn(product);
        lenient().when(favoriteRepository.existsByUserAndProduct(any(User.class), any(Product.class)))
                .thenReturn(true);

        // When, Then
        assertThrows(ResponseStatusException.class,
                () -> productService.getFavorite(principal, product.getId()));
    }

    @Test
    @Rollback
    void testDeleteFavorite_InvalidFavorite() {
        // Given
        Principal principal = () -> "user@example.com";
        User user = new User();
        Product product = new Product();
        Favorite favorite = new Favorite();
        List<Favorite> favorites = new ArrayList<>();
        favorites.add(favorite);
        favorite.setProduct(product);
        favorite.setUser(user);
        user.setFavorites(favorites);
        product.setFavorites(favorites);

        // Mocking
        when(utilsService.getUserByPrincipal(principal)).thenReturn(user);
        when(utilsService.getProductById(product.getId())).thenReturn(product);
        lenient().when(favoriteRepository.findByUserAndProduct(any(User.class), any(Product.class)))
                .thenReturn(favorite);

        // Then
        assertThrows(ResponseStatusException.class,
                () -> productService.deleteFavorite(principal, product.getId()));
    }

    private Product mockProduct() {
        List<ProductPhoto> photo = new ArrayList<>();
        Category category = new Category(1L, "dolls", "Ляльки, Пупси");
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

    private ProductRequest mockProductRequest() {
        return new ProductRequest
                ("Test Product",
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
