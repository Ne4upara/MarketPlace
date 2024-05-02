package ua.marketplace.requests;

import jakarta.validation.constraints.*;

/**
 * Represents a request object for submitting product ratings. This class uses the record syntax,
 * introduced in Java 14, which automatically generates the constructor, getters, equals, hashCode,
 * and toString methods.
 */
public record ProductRatingRequest(

        @NotNull(message = "Rating don't can be 'null'")
        @Positive(message = "Rating must be positive")
        @Min(value = 0, message = "Rating should be between 0 and 5")
        @Max(value = 5, message = "Rating should be between 0 and 5")
        int rating,

        @Size(max = 250, message = "Review should be max 250 characters")
        String review) {
}
