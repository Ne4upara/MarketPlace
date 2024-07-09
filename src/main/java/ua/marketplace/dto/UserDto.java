package ua.marketplace.dto;

import java.util.List;

/**
 * Represents a DTO (Data Transfer Object) for a user.
 * Contains information about the user, including their ID,
 * phone number, first name, list of favorite product IDs,
 * and an order list for the main page.
 */
public record UserDto(
        Long id,
        String phoneNumber,
        String firstName,
        List<Long> favorite_id,
        OrderListUserInfoDto order_list
) {
}
