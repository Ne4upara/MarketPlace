// Interface for the Product Controller with CRUD operations for products
@Tag(name = "Product controller", description = "Endpoints for CRUD operations for products")
public interface IProductController {

    // Get all products for the main page with pagination, sorting, and filtering options
    @Operation(summary = "Get all products for main page", 
            description = "Endpoint to retrieve all products for the main page. " +
                    "Sort by creationDate, productName, productPrice, or id.")
    @ApiResponse(responseCode = "200", description = "Successful operation")
    Pagination getAllProductsForMainPage(
            // Pagination parameters
            @Valid @RequestParam(defaultValue = "0") @PositiveOrZero int number,
            @Valid @RequestParam(defaultValue = "10") @Positive int size,
            // Sorting parameters
            @Valid @RequestParam(defaultValue = "creationDate")
            @Pattern(regexp = "creationDate|productName|productPrice|id") String sort,
            @Valid @RequestParam(defaultValue = "DESC") @Pattern(regexp = "ASC|DESC") String order);

    // Get products by category with pagination, sorting, and filtering options
    @Operation(summary = "Get products details by category", 
            description = "Endpoint to retrieve products by category. " +
                    "Sort by creationDate, productName, productPrice, or id.")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Successful operation",
                    content = @Content(schema = @Schema(implementation = Pagination.class))),
            @ApiResponse(responseCode = "409", description = "Invalid category",
                    content = @Content(schema = @Schema(implementation = ErrorMessageResponse.class)))
    })
    Pagination getAllProductsByCategory(
            // Pagination parameters
            @Valid @RequestParam(defaultValue = "0") @PositiveOrZero int number,
            @Valid @RequestParam(defaultValue = "10") @Positive int size,
            // Sorting parameters
            @Valid @RequestParam(defaultValue = "creationDate")
            @Pattern(regexp = "creationDate|productName|productPrice|id") String sort,
            @Valid @RequestParam(defaultValue = "DESC") @Pattern(regexp = "ASC|DESC") String order,
            // Category parameter
            @Parameter String category);

    // Get product details by ID
    @Operation(summary = "Get product details by ID", 
            description = "Endpoint to retrieve product details by ID")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Successful operation",
                    content = @Content(schema = @Schema(implementation = ProductDto.class))),
            @ApiResponse(responseCode = "404", description = "Product not found",
                    content = @Content(schema = @Schema(implementation = ErrorMessageResponse.class)))
    })
    ProductDto getProductDetailsById(@Parameter(description = "ID of the product") Long id);

    // Create a new product with validation and authorization checks
    @Operation(summary = "Create a new product", description = "Endpoint to create a new product")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Product created successfully"),
            @ApiResponse(responseCode = "400", description = "Invalid input",
                    content = @Content(schema = @Schema(implementation = ValidationErrorResponse.class))),
            @ApiResponse(responseCode = "401", description = "Not authorized user",
                    content = @Content())
    })
    ProductDto createProduct(
            // Principal object representing the authenticated user
            @Parameter(description = "Principal object representing the authenticated user") Principal principal,
            // Request body containing product details
            @Parameter(description = "Request body containing product details", schema =
            @Schema(implementation = ProductRequest.class))
            @Valid @RequestBody ProductRequest request);

    // Update an existing product with validation and authorization checks
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
                             // ID of the product to be updated
                             @Parameter(description = "ID of the product to be updated") Long id,
                             // Request body containing updated product details
                             @Parameter(description = "Request body containing updated product details",
                                     schema = @Schema(implementation = ProductRequest.class))
                             @Valid @RequestBody ProductRequest request);

    // Delete a product with validation and authorization checks
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
                       // ID of the product to be deleted
                       @Parameter(description = "ID of the product to be deleted") Long id);

    // Get all products created by the authenticated user with pagination, sorting, and filtering options
    @Operation(summary = "Get all my products.", 
            description = "Endpoint to retrieve all my products." +
                    "Sort by creationDate, productName, productPrice, or id.")
    @ApiResponses(value =  {
            @ApiResponse(responseCode = "200", description = "Successful operation"),
            @ApiResponse(responseCode = "403", description = "JWT token is missing")
    })
    Pagination getViewMyProduct(
            // Pagination parameters
            @Valid @RequestParam(defaultValue = "0") @PositiveOrZero int number,
            @Valid @RequestParam(defaultValue = "10") @Positive int size,
            // Sorting parameters
            @Valid @RequestParam(defaultValue = "creationDate")
            @Pattern(regexp = "creationDate|productName|productPrice|id") String sort,
            @Valid @RequestParam(defaultValue = "ASC") @Pattern(regexp = "ASC|DESC") String order,
            // Principal object representing the authenticated user
            Principal principal);

}
