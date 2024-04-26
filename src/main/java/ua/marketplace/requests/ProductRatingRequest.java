package ua.marketplace.requests;

import jakarta.validation.constraints.*;

/**
 * Represents a request object for submitting product ratings. This class uses the record syntax,
 * introduced in Java 14, which automatically generates the constructor, getters, equals, hashCode,
 * and toString methods.
 */
public record ProductRatingRequest(
        // The 'rating' field stores the user's rating for a product. It must be non-null, positive,
        // and between 0 and 5 (inclusive).
        @NotNull(message = "Rating don't can be 'null'")
        @Positive(message = "Rating must be positive")
        @Min(value = 0, message = "Rating should be between 0 and 5")
        @Max(value = 5, message = "Rating should be between 0 and 5")
        int rating,

        // The 'review' field stores the user's textual review of the product. It can be at most 250
        // characters long.
        @Size(max = 250, message = "Review should be max 250 characters")
        String review) {

}
