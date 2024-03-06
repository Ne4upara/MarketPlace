package ua.marketplace.services;

import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.requests.ProductRequest;

import java.security.Principal;
import java.util.List;

/**
 * Interface defining the contract for product-related operations.
 */
public interface IProductService {

    /**
     * Retrieves all products for the main page.
     *
     * @return List of MainPageProductDto containing product details for the main page.
     */
    List<MainPageProductDto> getAllProductsForMainPage();

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
}
