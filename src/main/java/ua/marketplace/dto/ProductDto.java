package ua.marketplace.dto;

import ua.marketplace.entities.ProductRating;
import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

/**
 * Data Transfer Object (DTO) representing product details.
 * This class provides an immutable representation of product information.
 */
public record ProductDto(
        // The unique identifier for the product
        Long id,

        // The name of the product
        String productName,

        // A list of photo links for the product
        List<String> productPhotoLink,

        // The price of the product
        BigDecimal productPrice,

        // A description of the product
        String productDescription,

        // The category of the product
        String productCategory,

        // The type of the product
        String productType,

        // The creation date and time of the product
        LocalDateTime creationDate,

        // The average rating of the product
        int rating,

        // A list of product ratings and reviews
        List<ProductRating> reviews,

        // The quantity of the product in stock
        int productQuantity,

        // The name of the product's seller
        String sellerName,

        // The phone number of the product's seller
        String sellerPhoneNumber,

        // The email address of the product's seller
        String sellerEmail,

        // The location of the product's seller
        String location,

        // The number of times the product details have been viewed
        int countView
) implements Serializable {

}
