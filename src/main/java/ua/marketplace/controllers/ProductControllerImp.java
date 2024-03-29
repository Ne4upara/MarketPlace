package ua.marketplace.controllers;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import ua.marketplace.dto.Pagination;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.requests.ProductRequest;
import ua.marketplace.services.ProductService;

import java.security.Principal;

/**
 * Controller class for managing product-related operations.
 */
@RestController
@RequestMapping("/api/v1/products")
@RequiredArgsConstructor
public class ProductControllerImp implements IProductController {

    private final ProductService productService;

    /**
     * Retrieves all products for the main page.
     *
     * @return List of MainPageProductDto containing product details for the main page.
     */
    @GetMapping("/s/list")
    @ResponseStatus(HttpStatus.OK)
    public Pagination getAllProductsForMainPage(
            @Valid @RequestParam(defaultValue = "0") @PositiveOrZero int number,
            @Valid @RequestParam(defaultValue = "10") @Positive int size,
            @Valid @RequestParam(defaultValue = "creationDate")
            @Pattern(regexp = "creationDate|productName|productPrice|id") String sort,
            @Valid @RequestParam(defaultValue = "DESC") @Pattern(regexp = "ASC|DESC") String order) {
        return productService.getAllProductsForMainPage(number, size, sort, order);
    }

    /**
     * Retrieves details of a product by its ID.
     *
     * @param id The ID of the product to retrieve.
     * @return ProductDto containing details of the product.
     */
    @GetMapping("/s/details/{id}")
    @ResponseStatus(HttpStatus.OK)
    public ProductDto getProductDetailsById(@PathVariable Long id) {
        return productService.getProductDetails(id);
    }

    /**
     * Creates a new product.
     *
     * @param principal The principal (typically representing the logged-in user).
     * @param request   ProductRequest containing details of the product to be created.
     * @return ProductDto containing details of the newly created product.
     */
    @PostMapping("/create")
    @ResponseStatus(HttpStatus.CREATED)
    public ProductDto createProduct(Principal principal, @Valid @RequestBody ProductRequest request) {
        return productService.saveProduct(principal, request);
    }

    /**
     * Updates an existing product.
     *
     * @param principal The principal (typically representing the logged-in user).
     * @param id        The ID of the product to be updated.
     * @param request   ProductRequest containing details to update the product.
     * @return ProductDto containing details of the updated product.
     */
    @PutMapping("/update/{id}")
    @ResponseStatus(HttpStatus.OK)
    public ProductDto updateProduct
    (Principal principal, @PathVariable Long id, @Valid @RequestBody ProductRequest request) {
        return productService.updateProduct(principal, id, request);
    }

    /**
     * Rates a product.
     *
     * @param productId The ID of the product to rate.
     * @param rating    The rating to assign to the product.
     * @return ProductDto containing details of the rated product.
     */
    @PatchMapping("/{productId}/rate/{rating}")
    @ResponseStatus(HttpStatus.OK)
    public ProductDto rateProduct(Principal principal, @PathVariable Long productId, @PathVariable int rating) {
        return productService.rateProduct(principal, productId, rating);
    }

    /**
     * Deletes a product.
     *
     * @param principal The principal (typically representing the logged-in user).
     * @param id        The ID of the product to be deleted.
     */
    @DeleteMapping("/delete/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void deleteProduct(Principal principal, @PathVariable Long id) {
        productService.deleteProduct(principal, id);
    }
}
