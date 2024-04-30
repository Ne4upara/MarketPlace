package ua.marketplace.controllers;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpSession;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import org.springframework.web.bind.annotation.*;
import ua.marketplace.dto.Pagination;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.requests.ProductRequest;
import ua.marketplace.swagger.responses.ErrorMessageResponse;
import ua.marketplace.swagger.responses.ValidationErrorResponse;

import java.security.Principal;

@Tag(name = "Product controller",
        description = "Endpoints for CRUD operations for products")
public interface IProductController {

    @Operation(summary = "Get all products for main page",
            description = "Endpoint to retrieve all products for the main page." +
                    "Sort -> creationDate, productName, productPrice, id. Order -> ASC. DESC.")
    @ApiResponse(responseCode = "200", description = "Successful operation")
    Pagination getAllProductsForMainPage(
            @Valid @RequestParam(defaultValue = "0") @PositiveOrZero int number,
            @Valid @RequestParam(defaultValue = "10") @Positive int size,
            @Valid @RequestParam(defaultValue = "creationDate")
            @Pattern(regexp = "creationDate|productName|productPrice|id") String sort,
            @Valid @RequestParam(defaultValue = "DESC") @Pattern(regexp = "ASC|DESC") String order);

    @Operation(summary = "Get products details by category",
            description = "Endpoint to retrieve products by category. " +
                    "Sort -> creationDate, productName, productPrice, id. Order -> ASC. DESC.")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Successful operation",
                    content = @Content(schema = @Schema(implementation = Pagination.class))),
            @ApiResponse(responseCode = "409", description = "Invalid category",
                    content = @Content(schema = @Schema(implementation = ErrorMessageResponse.class)))
    })
    Pagination getAllProductsByCategory(
            @Valid @RequestParam(defaultValue = "0") @PositiveOrZero int number,
            @Valid @RequestParam(defaultValue = "10") @Positive int size,
            @Valid @RequestParam(defaultValue = "creationDate")
            @Pattern(regexp = "creationDate|productName|productPrice|id") String sort,
            @Valid @RequestParam(defaultValue = "DESC") @Pattern(regexp = "ASC|DESC") String order,
            @Parameter String category);

    @Operation(summary = "Get product details by ID",
            description = "Endpoint to retrieve product details by ID")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Successful operation",
                    content = @Content(schema = @Schema(implementation = ProductDto.class))),
            @ApiResponse(responseCode = "404", description = "Product not found",
                    content = @Content(schema = @Schema(implementation = ErrorMessageResponse.class)))
    })
    ProductDto getProductDetailsById(@Parameter(description = "ID of the product") Long id, HttpSession session);

    @Operation(summary = "Create a new product", description = "Endpoint to create a new product")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Product created successfully"),
            @ApiResponse(responseCode = "400", description = "Invalid input",
                    content = @Content(schema = @Schema(implementation = ValidationErrorResponse.class))),
            @ApiResponse(responseCode = "401", description = "Not authorized user",
                    content = @Content())
    })
    ProductDto createProduct
            (@Parameter(description = "Principal object representing the authenticated user") Principal principal,
             @Parameter(description = "Request body containing product details", schema =
             @Schema(implementation = ProductRequest.class))
             @Valid @RequestBody ProductRequest request);

    @Operation(summary = "Update an existing product", description = "Endpoint to update an existing product")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Product updated successfully"),
            @ApiResponse(responseCode = "400", description = "Invalid input",
                    content = @Content(schema = @Schema(implementation = ValidationErrorResponse.class))),
            @ApiResponse(responseCode = "404", description = "Product not found",
                    content = @Content(schema = @Schema(implementation = ErrorMessageResponse.class))),
            @ApiResponse(responseCode = "401", description = "User not authorized",
                    content = @Content()),
            @ApiResponse(responseCode = "409", description = "This product was not created by this user",
                    content = @Content(schema = @Schema(implementation = ErrorMessageResponse.class)))

    })
    ProductDto updateProduct(@Parameter(description = "Principal object representing the authenticated user")
                             Principal principal,
                             @Parameter(description = "ID of the product to be updated") Long id,
                             @Parameter(description = "Request body containing updated product details",
                                     schema = @Schema(implementation = ProductRequest.class))
                             @Valid @RequestBody ProductRequest request);

    @Operation(summary = "Delete a product", description = "Endpoint to delete a product")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Product deleted successfully"),
            @ApiResponse(responseCode = "401", description = "User not authorized",
                    content = @Content()),
            @ApiResponse(responseCode = "404", description = "Product not found",
                    content = @Content(schema = @Schema(implementation = ErrorMessageResponse.class))),
            @ApiResponse(responseCode = "409", description = "This product was not created by this user",
                    content = @Content(schema = @Schema(implementation = ErrorMessageResponse.class)))
    })
    void deleteProduct(@Parameter(description = "Principal object representing the authenticated user")
                       Principal principal,
                       @Parameter(description = "ID of the product to be deleted") Long id);

    @Operation(summary = "Add product to favorite.",
            description = "Endpoint to add product to favorite.")
    @ApiResponses(value =  {
            @ApiResponse(responseCode = "204", description = "Successful operation"),
            @ApiResponse(responseCode = "400", description = "You cannot add a product a second time"
            ,content = @Content(schema = @Schema(implementation = ErrorMessageResponse.class)))
    })
    void getFavoriteProduct(Principal principal, @PathVariable Long id);


    @Operation(summary = "Delete product favorite.",
            description = "Endpoint to delete product is favorite.")
    @ApiResponses(value =  {
            @ApiResponse(responseCode = "204", description = "Successful operation"),
            @ApiResponse(responseCode = "400", description = "You cannot remove a non-existent product"
                    , content = @Content(schema = @Schema(implementation = ErrorMessageResponse.class)))
    })
    void deleteFavorite(Principal principal, @PathVariable Long id);
}
