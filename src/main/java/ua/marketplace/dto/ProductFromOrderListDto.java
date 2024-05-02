package ua.marketplace.dto;

import java.math.BigDecimal;

/**
 * Represents a DTO (Data Transfer Object) for a product within an order list.
 * Contains information about the product, including its ID, photo link,
 * name, and price.
 */
public record ProductFromOrderListDto(
        Long id,
        String productPhotoLink,
        String productName,
        BigDecimal productPrice
) {
}
