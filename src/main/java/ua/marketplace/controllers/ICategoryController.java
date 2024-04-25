package ua.marketplace.controllers;

import io.swagger.v3.oas.annotations.Operation; // Imported for API documentation using OpenAPI 3.0
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import ua.marketplace.entities.Category; // Imported Category entity class

import java.util.List; // Imported List interface for handling lists of Category objects

/**
 * ICategoryController interface defines the contract for CategoryController.
 * It contains a single method to retrieve all product categories.
 */
public interface ICategoryController {

    /**
     * getAllCategory() method retrieves all product categories from the system.
     * It returns a list of Category objects, each containing id, url link, and name for catalog view.
     *
     * <p>OpenAPI 3.0 annotations are used for API documentation:</p>
     * <ul>
     *     <li>Operation: Provides a summary and description for the endpoint.</li>
     *     <li>ApiResponses: Provides a list of possible API responses with response codes and descriptions.</li>
     *     <li>ApiResponse: Provides a specific API response with a response code, description, and content type.</li>
     * </ul>
     *
     * @return A list of Category objects representing all product categories in the system.
     */
    @Operation(summary = "Get all Category", // Summary of the endpoint
            description = "Endpoint to retrieve all product categories (id, url link, name for catalog view.)") // Detailed description of the endpoint
    @ApiResponses(value = { // List of possible API responses
            @ApiResponse(responseCode = "200", // Response code for successful operation
                    description = "Successful operation", // Description of the successful operation
                    content = @Content(array = @ArraySchema(schema = @Schema(implementation = Category.class)))) // Content type of the response (array of Category objects)
    })
    List<Category> getAllCategory(); // Method to retrieve all product categories
}
