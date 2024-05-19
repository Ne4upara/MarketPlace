package ua.marketplace.services;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.test.annotation.Rollback;
import org.springframework.test.context.TestPropertySource;
import org.springframework.transaction.annotation.Transactional;
import ua.marketplace.dto.Pagination;
import ua.marketplace.entities.Favorite;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.User;
import ua.marketplace.repositoryes.FavoriteRepository;
import ua.marketplace.repositoryes.ProductRepository;
import ua.marketplace.services.impl.UserService;
import ua.marketplace.services.impl.UtilsService;

import java.security.Principal;
import java.util.Collections;
import java.util.List;

import static org.hibernate.validator.internal.util.Contracts.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@TestPropertySource(locations="classpath:application-dev.properties")
@Transactional
class UserServiceTest {

    @Mock
    private UtilsService utilsService;

    @Mock
    private ProductRepository productRepository;

    @Mock
    private FavoriteRepository favoriteRepository;

    @InjectMocks
    private UserService userService;

    @Test
    @Rollback
    void testGetViewMyProduct() {
        // Given
        int pageNumber = 0;
        int pageSize = 10;
        String sortBy = "productName";
        String orderBy = "ASC";
        Principal principal = () -> "user@example.com";

        User user = new User();
        when(utilsService.getUserByPrincipal(principal)).thenReturn(user);

        Page<Product> productPage = mock(Page.class);

        when(productPage.getNumber()).thenReturn(pageNumber);
        when(productPage.getTotalElements()).thenReturn(1L);
        when(productPage.getTotalPages()).thenReturn(1);
        when(productRepository.findAllByOwner(user, utilsService.getPageRequest(pageNumber, pageSize, sortBy, orderBy)))
                .thenReturn(productPage);

        // When
        Pagination result = userService.getViewMyProduct(pageNumber, pageSize, sortBy, orderBy, principal);

        // Then
        assertNotNull(result);
        assertEquals(pageNumber, result.pageNumber());
        assertEquals(1L, result.totalElements());
        assertEquals(1, result.totalPages());
    }


    @Test
    @Rollback
    void testGetAllFavorite() {
        // Given
        int pageNumber = 0;
        int pageSize = 10;
        String sortBy = "productName";
        String orderBy = "ASC";
        Principal principal = () -> "user@example.com";

        User user = new User();
        when(utilsService.getUserByPrincipal(principal)).thenReturn(user);

        Page<Favorite> favoritePage = mock(Page.class);
        List<Favorite> favorites = Collections.singletonList(new Favorite());
        lenient().when(favoritePage.getContent()).thenReturn(favorites);
        lenient().when(favoritePage.getNumber()).thenReturn(pageNumber);
        lenient().when(favoritePage.getTotalElements()).thenReturn(1L);
        lenient().when(favoritePage.getTotalPages()).thenReturn(1);

        when(favoriteRepository.findAllByUser(user, utilsService.getPageRequest(pageNumber, pageSize, sortBy, orderBy)))
                .thenReturn(favoritePage);

        Page<Product> productPage = mock(Page.class);


        // When
        Pagination result = userService.getAllFavorite(pageNumber, pageSize, sortBy, orderBy, principal);

        // Then
        assertEquals(pageNumber, result.pageNumber());
        assertEquals(1L, result.totalElements());
        assertEquals(1, result.totalPages());
    }
}
