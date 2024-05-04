package ua.marketplace.controllers;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import org.springframework.web.bind.annotation.RequestParam;
import ua.marketplace.dto.Pagination;
import ua.marketplace.dto.UserDto;
import ua.marketplace.swagger.responses.ErrorMessageResponse;

import java.security.Principal;

@Tag(name = "User controller",
        description = "Endpoints for CRUD operations for users")
public interface IUserController {

    @Operation(summary = "Get all my products.",
            description = "Endpoint to retrieve all my products." +
                    " Sort -> creationDate, productName, productPrice, id. Order -> ASC. DESC.")
    @ApiResponses(value =  {
            @ApiResponse(responseCode = "200", description = "Successful operation"),
            @ApiResponse(responseCode = "403", description = "JWT token is missing", content = @Content())
    })
    Pagination getViewMyProduct(
            @Valid @RequestParam(defaultValue = "0") @PositiveOrZero int number,
            @Valid @RequestParam(defaultValue = "10") @Positive int size,
            @Valid @RequestParam(defaultValue = "creationDate")
            @Pattern(regexp = "creationDate|productName|productPrice|id") String sort,
            @Valid @RequestParam(defaultValue = "ASC") @Pattern(regexp = "ASC|DESC") String order,
            Principal principal);

    @Operation(summary = "Get all info user.",
            description = "Endpoint get all info user.")
    @ApiResponses(value =  {
            @ApiResponse(responseCode = "200", description = "Successful operation"),
            @ApiResponse(responseCode = "401", description = "User not authorize", content = @Content(
                    schema = @Schema(implementation = ErrorMessageResponse.class))),
            @ApiResponse(responseCode = "403", description = "JWT token is missing", content = @Content())
    })
    UserDto getUserInfo(Principal principal);

    @Operation(summary = "Get all user's favorite products.",
            description = "Endpoint get all user's favorite products." +
                    " Sort -> creationDate, productName, productPrice, id. Order -> ASC. DESC.")
    @ApiResponses(value =  {
            @ApiResponse(responseCode = "200", description = "Successful operation"),
            @ApiResponse(responseCode = "403", description = "JWT token is missing",content = @Content())
    })
    Pagination getAllFavorite(
            @Valid @RequestParam(defaultValue = "0") @PositiveOrZero int number,
            @Valid @RequestParam(defaultValue = "10") @Positive int size,
            @Valid @RequestParam(defaultValue = "creationDate")
            @Pattern(regexp = "creationDate|productName|productPrice|id") String sort,
            @Valid @RequestParam(defaultValue = "ASC") @Pattern(regexp = "ASC|DESC") String order,
            Principal principal);
}
