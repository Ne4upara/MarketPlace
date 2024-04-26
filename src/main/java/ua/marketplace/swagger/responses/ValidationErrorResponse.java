package ua.marketplace.swagger.responses;

import io.swagger.v3.oas.annotations.media.Schema;

import java.util.Map;

public record ValidationErrorResponse(
        @Schema(description = "Validation errors")
        Map<String, String> errorMessage
) {

}
