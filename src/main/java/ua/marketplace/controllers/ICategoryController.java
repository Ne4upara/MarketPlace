package ua.marketplace.controllers;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import ua.marketplace.entities.Category;

import java.util.List;

public interface ICategoryController {

    @Operation(summary = "Get all Category",
            description = "Endpoint to retrieve all product categories (id, url link, name for catalog view.")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Successful operation",
                    content = @Content(array = @ArraySchema(schema = @Schema(implementation = Category.class)))),
    })
    List<Category> getAllCategory();

}
