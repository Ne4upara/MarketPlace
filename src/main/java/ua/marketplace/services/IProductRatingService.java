package ua.marketplace.services;

import ua.marketplace.dto.ProductRatingDto; // ProductRatingDto is used to transfer data for Product Rating
import ua.marketplace.requests.ProductRatingRequest; // ProductRatingRequest is used to request data for Product Rating

import java.security.Principal; // Principal is used for authentication
/**
 * Interface for managing product ratings.
 * Provides methods for rating, updating ratings, and deleting ratings for products.
 */
//TODO
public interface IProductRatingService {

    /**
     * Rates a product based on the given request.
     *
     * @param principal The principal representing the authenticated user.
     * @param id The ID of the product to rate.
     * @param request The request containing rating information.
     * @return The DTO representing the rated product.
     */
    ProductRatingDto rateProduct(Principal principal, Long id, ProductRatingRequest request);

    /**
     * Updates the rating for a product based on the given request.
     *
     * @param principal The principal representing the authenticated user.
     * @param id The ID of the product to update the rating for.
     * @param request The request containing updated rating information.
     * @return The DTO representing the updated rating for the product.
     */
    ProductRatingDto updateRating(Principal principal, Long id, ProductRatingRequest request);

    /**
     * Deletes the rating for a product.
     *
     * @param principal The principal representing the authenticated user.
     * @param id The ID of the product to delete the rating for.
     */
    void deleteRating(Principal principal, Long id);
}
