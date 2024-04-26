package ua.marketplace.dto;

import java.time.LocalDateTime;

/**
 * A Data Transfer Object (DTO) for representing product rating information.
 * This class contains the following fields:
 * 
 * - `id`: The unique identifier of the product rating.
 * - `productId`: The identifier of the product being rated.
 * - `username`: The username of the user who provided the rating.
 * - `rating`: The numerical rating given to the product, between 1 and 5.
 * - `review`: A textual review of the product, if provided.
 * - `createAt`: The date and time when the rating was created.
 */
public record ProductRatingDto(
        Long id,
        Long productId,
        String username,
        int rating,
        String review,
        LocalDateTime createAt
) {

}

