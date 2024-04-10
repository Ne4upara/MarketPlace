package ua.marketplace.dto;

import java.math.BigDecimal;

/**
 * Data Transfer Object (DTO) representing product details for the main page.
 * This class provides an immutable representation of product information for main page.
 */
public record MainPageProductDto(
        Long id,
        String productPhotoLink,
        String productName,
        String productType,
        BigDecimal productPrice,
        int productRating,
        int countView
) {
}
