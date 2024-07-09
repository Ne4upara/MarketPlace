package ua.marketplace.swagger.responses;

import io.swagger.v3.oas.annotations.media.Schema;

import java.util.Map;

/**
 * ValidationErrorResponse is a record class that represents a response containing validation errors.
 * It contains a single field, errorMessage, which is a map of error messages associated with
 * specific fields in the request.
 */
public record ValidationErrorResponse(
        @Schema(description = "Validation errors")
        Map<String, String> errorMessage
) {
}

