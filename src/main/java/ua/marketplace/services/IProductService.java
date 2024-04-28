package ua.marketplace.services;

import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.cache.annotation.Caching;
import ua.marketplace.dto.Pagination;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.requests.ProductRequest;

import java.security.Principal;

/**
 * Interface defining the contract for product-related operations.
 */
public interface IProductService {

    /**
     * Retrieves details of a product by its ID.
     *
     * @param id The ID of the product to retrieve.
     * @return ProductDto containing details of the product.
     */
    @Cacheable(value = "product", key = "#id")
    ProductDto getProductDetails(Long id);

    /**
     * Saves a new product.
     *
     * @param principal The principal (typically representing the logged-in user).
     * @param request   ProductRequest containing details of the product to be saved.
     * @return ProductDto containing details of the newly saved product.
     */
    @Caching(evict = {
            @CacheEvict(value = "product", key = "#result.id"),
            @CacheEvict(value = "products", allEntries = true)
    })
    ProductDto saveProduct(Principal principal, ProductRequest request);

    /**
     * Updates an existing product.
     *
     * @param principal The principal (typically representing the logged-in user).
     * @param productId The ID of the product to be updated.
     * @param request   ProductRequest containing details to update the product.
     * @return ProductDto containing details of the updated product.
     */
    @Caching(evict = {
            @CacheEvict(value = "product", key = "#productId"),
            @CacheEvict(value = "products", allEntries = true)
    })
    ProductDto updateProduct(Principal principal, Long productId, ProductRequest request);

    /**
     * Deletes a product.
     *
     * @param principal The principal (typically representing the logged-in user).
     * @param productId The ID of the product to be deleted.
     */
    @Caching(evict = {
            @CacheEvict(value = "product", key = "#productId"),
            @CacheEvict(value = "products", allEntries = true) // Clear all products cache
    })
    void deleteProduct(Principal principal, Long productId);

    /**
     * Retrieves products paginated for the main page.
     *
     * @param pageNumber The page number to retrieve.
     * @param pageSize   The number of products per page.
     * @param sortBy     The field to sort the products by (e.g., "creationDate", "productName", "productPrice", "id").
     * @param orderBy    The sorting order ("ASC" for ascending, "DESC" for descending).
     * @return Pagination object containing the paginated list of products for the main page.
     */
    @Cacheable(value = "products", key = "{#pageNumber, #pageSize, #sortBy, #orderBy}")
    Pagination getAllProductsForMainPage(int pageNumber, int pageSize, String sortBy, String orderBy);

    /**
     * Retrieves products paginated filtered by category.
     *
     * @param pageNumber The page number to retrieve.
     * @param pageSize   The number of products per page.
     * @param sortBy     The field to sort the products by (e.g., "creationDate", "productName", "productPrice", "id").
     * @param orderBy    The sorting order ("ASC" for ascending, "DESC" for descending).
     * @param category   The category by which to filter the products.
     * @return Pagination object containing the paginated list of products for the main page filtered by category.
     */
    @Cacheable(value = "productsByCategory", key = "{#pageNumber, #pageSize, #sortBy, #orderBy, #category}")
    Pagination getAllProductsByCategory
    (int pageNumber, int pageSize, String sortBy, String orderBy, String category);

    void getFavorite(Principal principal, Long id);

    void deleteFavorite(Principal principal, Long id);
}
