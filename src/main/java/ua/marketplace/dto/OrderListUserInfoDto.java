package ua.marketplace.dto;

import java.math.BigDecimal;

/**
 * Represents a DTO (Data Transfer Object) for an order list on the main page.
 * Contains summarized information about the order list, including its ID,
 * the count of products in the order, and the total price.
 */
public record OrderListUserInfoDto(
        Long id,
        int count,
        BigDecimal totalPrice
) {
}
