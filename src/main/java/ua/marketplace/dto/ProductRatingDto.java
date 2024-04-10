package ua.marketplace.dto;

import java.time.LocalDateTime;

public record ProductRatingDto(
        Long id,
        Long productId,
        String username,
        int rating,
        String review,
        LocalDateTime createAt
) {
}
