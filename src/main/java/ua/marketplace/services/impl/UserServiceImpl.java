package ua.marketplace.services.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.dto.Pagination;
import ua.marketplace.dto.UserDto;
import ua.marketplace.entities.Favorite;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.User;
import ua.marketplace.mapper.UserMapper;
import ua.marketplace.repositoryes.FavoriteRepository;
import ua.marketplace.repositoryes.ProductRepository;
import ua.marketplace.services.UserService;

import java.security.Principal;
import java.util.List;
import java.util.Set;

/**
 * Service class for managing user-related operations.
 * Implements the {@link ua.marketplace.services.UserService} interface.
 */
@Service
@RequiredArgsConstructor
public class UserServiceImpl implements UserService {

    private final UtilsService utilsService;
    private final ProductRepository productRepository;
    private final FavoriteRepository favoriteRepository;

    /**
     * Retrieves products paginated for viewing all user products.
     *
     * @param pageNumber The page number to retrieve.
     * @param pageSize   The number of products per page.
     * @param sortBy     The field to sort the products by.
     * @param orderBy    The sorting order.
     * @param principal  The principal representing the logged-in user.
     * @return Pagination object containing the paginated list of products for the main page.
     */
    @Override
    public Pagination getMyProducts(
            int pageNumber, int pageSize, String sortBy, String orderBy, Principal principal) {
        User user = utilsService.getUserByPrincipal(principal);
        Pageable pageable = utilsService.getPageRequest(pageNumber, pageSize, sortBy, orderBy);
        Page<Product> pageAll = findAllByOwner(user, pageable);
        List<MainPageProductDto> pageAllContent = utilsService.convertProductListToDto(pageAll);
        return createPagination(pageAll, pageAllContent);
    }

    private Page<Product> findAllByOwner(User user,Pageable pageable) {
       return productRepository.findAllByOwner(user, pageable);
    }

    private Pagination createPagination (Page<Product> page, List<MainPageProductDto> content) {
        return new Pagination(page.getNumber(),
                page.getTotalElements(),
                page.getTotalPages(),
                content);
    }

    /**
     * Retrieves favorite products paginated for a user.
     *
     * @param pageNumber The page number to retrieve.
     * @param pageSize   The number of products per page.
     * @param sortBy     The field to sort the products by.
     * @param orderBy    The sorting order.
     * @param principal  The principal representing the logged-in user.
     * @return Pagination object containing the paginated list of favorite products for the user.
     */
    @Override
    public Pagination getAllFavoriteProducts(
            int pageNumber, int pageSize, String sortBy, String orderBy, Principal principal) {
        User user = utilsService.getUserByPrincipal(principal);
        Pageable pageable = utilsService.getPageRequest(pageNumber, pageSize, sortBy, orderBy);
        Page<Favorite> favoritesPage = getFavoritesPage(user, pageable);
        Page<Product> productFavorite = favoritesPage.map(Favorite::getProduct);
        List<MainPageProductDto> pageAllContent = utilsService.convertProductListToDto(productFavorite);
        return getPaginationByFavorite(favoritesPage, pageAllContent);
    }

    private Page<Favorite> getFavoritesPage(User user, Pageable pageable) {
        return favoriteRepository.findAllByUser(user, pageable);
    }

    private Pagination getPaginationByFavorite(Page<Favorite> favorites, List<MainPageProductDto> content) {
        return new Pagination(favorites.getNumber(),
                favorites.getTotalElements(),
                favorites.getTotalPages(),
                content);
    }

    /**
     * Retrieves information about the logged-in user.
     *
     * @param principal The principal representing the logged-in user.
     * @return UserDto object containing user information.
     */
    @Override
    public UserDto getUserInfo(Principal principal) {
        User user = utilsService.getUserByPrincipal(principal);
        Set<Favorite> favorites = getFavoritesByUser(user);
        return convertToUserDto(user, favorites);
    }

    private Set<Favorite> getFavoritesByUser(User user) {
        return favoriteRepository.findByUser(user);
    }

    private UserDto convertToUserDto(User user, Set<Favorite> favorites) {
        return UserMapper.USER_MAPPER.userToUserDTO(user, favorites);
    }
}
