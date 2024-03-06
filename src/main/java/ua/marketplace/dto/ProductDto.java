package ua.marketplace.dto;

import ua.marketplace.data.ProductCategory;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * Data Transfer Object (DTO) representing product details.
 * This class provides an immutable representation of product information.
 */
public record ProductDto(
        String productName,
        String productPhotoLink,
        BigDecimal productPrice,
        String productDescription,
        ProductCategory productCategory,
        String productType,
        LocalDateTime creationDate,
        int productRating,
        int productRatingCount,
        int productQuantity
) {
}
