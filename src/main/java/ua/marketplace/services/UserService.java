package ua.marketplace.services;

import org.springframework.cache.annotation.Cacheable;
import ua.marketplace.dto.Pagination;
import ua.marketplace.dto.UserDto;

import java.security.Principal;

/**
 * Service interface for managing user-related operations.
 */
public interface UserService {

    /**
     * Retrieves products paginated for view all my products.
     *
     * @param pageNumber The page number to retrieve.
     * @param pageSize   The number of products per page.
     * @param sortBy     The field to sort the products by (e.g., "creationDate", "productName", "productPrice", "id").
     * @param orderBy    The sorting order ("ASC" for ascending, "DESC" for descending).
     * @param principal  The principal (typically representing the logged-in user).
     * @return Pagination object containing the paginated list of products for the main page.
     */
    @Cacheable(value = "products", key = "{#pageNumber, #pageSize, #sortBy, #orderBy, #principal}")
    Pagination getMyProducts(
            int pageNumber, int pageSize, String sortBy, String orderBy, Principal principal);

    /**
     * Retrieves favorite products paginated for a user.
     *
     * @param pageNumber The page number to retrieve.
     * @param pageSize   The number of products per page.
     * @param sortBy     The field to sort the products by (e.g., "creationDate", "productName", "productPrice", "id").
     * @param orderBy    The sorting order ("ASC" for ascending, "DESC" for descending).
     * @param principal  The principal representing the logged-in user.
     * @return Pagination object containing the paginated list of favorite products for the user.
     */
    Pagination getAllFavoriteProducts(
            int pageNumber, int pageSize, String sortBy, String orderBy, Principal principal);

    /**
     * Retrieves information about the logged-in user.
     *
     * @param principal The principal representing the logged-in user.
     * @return UserDto object containing user information.
     */
    UserDto getUserInfo(Principal principal);
}
