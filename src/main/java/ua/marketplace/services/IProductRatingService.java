package ua.marketplace.services;

import ua.marketplace.dto.ProductRatingDto;
import ua.marketplace.requests.ProductRatingRequest;

import java.security.Principal;

public interface IProductRatingService {

    ProductRatingDto rateProduct(Principal principal, Long id, ProductRatingRequest request);
    ProductRatingDto updateRating(Principal principal, Long id, ProductRatingRequest request);
    void deleteRating(Principal principal, Long id);
}
