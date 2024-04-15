package ua.marketplace.controllers;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.RequestBody;
import ua.marketplace.dto.CodeDto;
import ua.marketplace.dto.PhoneNumberDto;
import ua.marketplace.requests.PhoneCodeRequest;
import ua.marketplace.requests.PhoneNumberRequest;
import ua.marketplace.requests.RegistrationRequest;
import ua.marketplace.swagger.responses.ErrorMessageResponse;
import ua.marketplace.swagger.responses.ValidationErrorResponse;

@Tag(name = "Authorization controller",
        description = "Endpoints for registration and authorization users")
public interface IPhoneAuthController {

    @Operation(summary = "User login",
            description = "Endpoint for sending user verification codes")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Successful operation",
                    content = @Content(schema = @Schema(implementation = PhoneNumberDto.class))),
            @ApiResponse(responseCode = "400", description = "Invalid input",
                    content = @Content(schema = @Schema(implementation = ValidationErrorResponse.class))),
            @ApiResponse(responseCode = "404", description = "User with this phone number not found",
                    content = @Content(schema = @Schema(implementation = ErrorMessageResponse.class))),
            @ApiResponse(responseCode = "409", description =
                    "The SMS code cannot be received more often than 1 time per 1 minute",
                    content = @Content(schema = @Schema(implementation = ErrorMessageResponse.class)))
    })
    PhoneNumberDto inputPhoneNumber(@Parameter(description = "Request body containing user phone number", schema =
    @Schema(implementation = PhoneNumberRequest.class))
                                    @Valid @RequestBody PhoneNumberRequest request);

    @Operation(summary = "Verification SMS-code",
            description = "Endpoint for verification SMS-code")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Successful operation",
                    content = @Content(schema = @Schema(implementation = CodeDto.class))),
            @ApiResponse(responseCode = "400", description = "The code was entered incorrectly",
                    content = @Content(schema = @Schema(implementation = ErrorMessageResponse.class))),
            @ApiResponse(responseCode = "409", description =
                    "The SMS code has already been entered / All login attempts have been used(3) " +
                            "/ SMS-code time is up(5 min)",
                    content = @Content(schema = @Schema(implementation = ErrorMessageResponse.class)))
    })
    CodeDto inputCode(@Parameter(description = "Request body containing user phone number", schema =
    @Schema(implementation = PhoneCodeRequest.class))
                      @Valid @RequestBody PhoneCodeRequest request);

    @Operation(summary = "Registration new user",
            description = "Endpoint for registration new users")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "202", description = "Successful operation",
                    content = @Content(schema = @Schema(implementation = PhoneNumberDto.class))),
            @ApiResponse(responseCode = "400", description = "Invalid input",
                    content = @Content(schema = @Schema(implementation = ValidationErrorResponse.class))),
            @ApiResponse(responseCode = "409", description =
                    "Phone already exist",
                    content = @Content(schema = @Schema(implementation = ErrorMessageResponse.class)))
    })
    PhoneNumberDto registration(@Parameter(description = "Request body containing user phone number", schema =
    @Schema(implementation = RegistrationRequest.class))
                                @Valid @RequestBody RegistrationRequest request);

    @Operation(summary = "Logout",
            description = "Endpoint for logout users")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Logout successfully")
    })
    void logout(HttpServletRequest request, HttpSession session);
}
