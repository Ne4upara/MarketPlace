package ua.marketplace.services;

import ua.marketplace.dto.ProductRatingDto; // ProductRatingDto is used to transfer data for Product Rating
import ua.marketplace.requests.ProductRatingRequest; // ProductRatingRequest is used to request data for Product Rating

import java.security.Principal; // Principal is used for authentication

public interface IProductRatingService {

    // Method to rate a product by a user
    ProductRatingDto rateProduct(Principal principal, Long id, ProductRatingRequest request);

    // Method to update an existing product rating by a user
    ProductRatingDto updateRating(Principal principal, Long id, ProductRatingRequest request);

    // Method to delete a product rating by a user
    void deleteRating(Principal principal, Long id);
}

