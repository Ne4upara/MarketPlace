package ua.marketplace.services;

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
    ProductDto getProductDetails(Long id);

    /**
     * Saves a new product.
     *
     * @param principal The principal (typically representing the logged-in user).
     * @param request   ProductRequest containing details of the product to be saved.
     * @return ProductDto containing details of the newly saved product.
     */
    ProductDto saveProduct(Principal principal, ProductRequest request);

    /**
     * Updates an existing product.
     *
     * @param principal The principal (typically representing the logged-in user).
     * @param productId The ID of the product to be updated.
     * @param request   ProductRequest containing details to update the product.
     * @return ProductDto containing details of the updated product.
     */
    ProductDto updateProduct(Principal principal, Long productId, ProductRequest request);

    /**
     * Rates a product.
     *
     * @param productId The ID of the product to rate.
     * @param rating    The rating to assign to the product.
     * @return ProductDto containing details of the rated product.
     */
    ProductDto rateProduct(Long productId, int rating);

    /**
     * Deletes a product.
     *
     * @param principal The principal (typically representing the logged-in user).
     * @param productId The ID of the product to be deleted.
     */
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
    Pagination getAllProductsForMainPage(int pageNumber, int pageSize, String sortBy, String orderBy);
}
