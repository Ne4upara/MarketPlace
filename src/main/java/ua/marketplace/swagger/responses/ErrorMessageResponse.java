package ua.marketplace.swagger.responses;

import io.swagger.v3.oas.annotations.media.Schema;


public record ErrorMessageResponse(
        @Schema(description = "Error message")
        String errorMessage
) {
}
