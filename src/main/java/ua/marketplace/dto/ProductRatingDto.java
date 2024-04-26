package ua.marketplace.dto;

import java.time.LocalDateTime;

/**
 * A DTO (Data Transfer Object) class representing product rating information.
 */
//TODO
public record ProductRatingDto(
        Long id,
        Long productId,
        String username,
        int rating,
        String review,
        LocalDateTime createAt
) {

}
