package ua.marketplace.dto;

/**
 * A DTO (Data Transfer Object) class representing authentication information.
 */
public record CodeDto(
        String token,
        String firstName
) {
}
