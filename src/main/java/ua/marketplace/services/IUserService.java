package ua.marketplace.services;

import org.springframework.cache.annotation.Cacheable;
import ua.marketplace.dto.Pagination;

import java.security.Principal;

public interface IUserService {

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
    Pagination getViewMyProduct (
            int pageNumber, int pageSize, String sortBy, String orderBy, Principal principal);
}
