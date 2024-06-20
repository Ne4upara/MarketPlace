package ua.marketplace.controllers;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import ua.marketplace.dto.OrderListDto;
import ua.marketplace.dto.OrderListUserInfoDto;

import java.security.Principal;

@Tag(name = "OrderListService controller",
        description = "Endpoints for add/delete/view operations for order list")
public interface OrderListController {

    @Operation(summary = "Get user order list",
            description = "Endpoint to view user order list")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Successful operation",
                    content = @Content(schema = @Schema(implementation = OrderListDto.class))),
            @ApiResponse(responseCode = "403", description = "User not authorized")
    })
    OrderListDto getOrderList (
            @Parameter(description = "Principal object representing the authenticated user") Principal principal);

    @Operation(summary = "Add product to order list",
            description = "Endpoint to add product to order list")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Successful operation",
                    content = @Content(schema = @Schema(implementation = OrderListUserInfoDto.class))),
            @ApiResponse(responseCode = "403", description = "User not authorized"),
            @ApiResponse(responseCode = "404", description = "Product with this ID not found")
    })
    OrderListUserInfoDto addProductToOrderList (
            @Parameter(description = "Principal object representing the authenticated user")Principal principal,
            @Parameter(description = "ID of the product to be added to order list") Long id);

    @Operation(summary = "Delete product to order list",
            description = "Endpoint to Delete product to order list")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Successful operation",
                    content = @Content(schema = @Schema(implementation = OrderListUserInfoDto.class))),
            @ApiResponse(responseCode = "403", description = "User not authorized"),
            @ApiResponse(responseCode = "404", description = "Product with this ID not found")
    })
    OrderListUserInfoDto deleteProductFromOrderList (
            @Parameter(description = "Principal object representing the authenticated user")Principal principal,
            @Parameter(description = "ID of the product to be deleted to order list")Long id);
}
