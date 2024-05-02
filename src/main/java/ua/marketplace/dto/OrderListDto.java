package ua.marketplace.dto;

import java.math.BigDecimal;
import java.util.List;

/**
 * Represents a DTO (Data Transfer Object) for an order list.
 * Contains information about the order list, including its ID,
 * the list of products in the order, and the total price.
 */
public record OrderListDto(
        Long id,
        List<ProductFromOrderListDto> products,
        BigDecimal totalPrice
) {
}
