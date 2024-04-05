package ua.marketplace.dto;

import ua.marketplace.entities.ProductRating;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

/**
 * Data Transfer Object (DTO) representing product details.
 * This class provides an immutable representation of product information.
 */
public record ProductDto(
        Long id,
        String productName,
        List<String> productPhotoLink,
        BigDecimal productPrice,
        String productDescription,
        String productCategory,
        String productType,
        LocalDateTime creationDate,
        int rating,
        List<ProductRating> reviews,
        int productQuantity
) {
}
