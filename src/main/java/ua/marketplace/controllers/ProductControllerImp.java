package ua.marketplace.controllers;

import io.micrometer.core.annotation.Counted;
import io.micrometer.core.annotation.Timed;
import jakarta.servlet.http.HttpSession;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import ua.marketplace.dto.Pagination;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.repositoryes.ProductRepository;
import ua.marketplace.requests.ProductRequest;
import ua.marketplace.services.ProductService;

import java.security.Principal;

/**
 * Controller class for managing product-related operations.
 */
@RestController
@RequestMapping("/v1/products")
@RequiredArgsConstructor
public class ProductControllerImp implements IProductController {

    private final ProductService productService;
    private final ProductRepository productRepository;

    /**
     * Retrieves all products for the main page.
     *
     * @return List of MainPageProductDto containing product details for the main page.
     */
    @GetMapping("/s/view")
    @Timed("getMainPageList")
    @ResponseStatus(HttpStatus.OK)
    @Counted(value = "get.list.request", description = "Number of requests to list all endpoint")
    public Pagination getAllProductsForMainPage(
            @Valid @RequestParam(defaultValue = "0") @PositiveOrZero int number,
            @Valid @RequestParam(defaultValue = "10") @Positive int size,
            @Valid @RequestParam(defaultValue = "creationDate")
            @Pattern(regexp = "creationDate|productName|productPrice|id") String sort,
            @Valid @RequestParam(defaultValue = "DESC") @Pattern(regexp = "ASC|DESC") String order) {
        return productService.getAllProductsForMainPage(number, size, sort, order);
    }

    /**
     * Retrieves all products by category.
     *
     * @param category We transfer the category to receive goods according to it
     * @return List of MainPageProductDto containing product details by category.
     */
    @GetMapping("/s/view/{category}")
    @ResponseStatus(HttpStatus.OK)
    @Timed("getCategoriesProduct")
    @Counted(value = "get.category.product", description = "Number request to category list")
    public Pagination getAllProductsByCategory(
            @Valid @RequestParam(defaultValue = "0") @PositiveOrZero int number,
            @Valid @RequestParam(defaultValue = "10") @Positive int size,
            @Valid @RequestParam(defaultValue = "creationDate")
            @Pattern(regexp = "creationDate|productName|productPrice|id") String sort,
            @Valid @RequestParam(defaultValue = "DESC") @Pattern(regexp = "ASC|DESC") String order,
            @PathVariable String category) {
        return productService.getAllProductsByCategory(number, size, sort, order, category);
    }

    /**
     * Retrieves details of a product by its ID.
     *
     * @param id The ID of the product to retrieve.
     * @return ProductDto containing details of the product.
     */
    @GetMapping("/s/view/details/{id}")
    @ResponseStatus(HttpStatus.OK)
    @Timed("getProductDetails")
    public ProductDto getProductDetailsById(@PathVariable Long id, HttpSession session) {
        productService.incrementViewProduct(session, id);
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
    @Timed("getCreateProduct")
    @ResponseStatus(HttpStatus.CREATED)
    @Counted(value = "create.request", description = "This counted for create request.")
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
    @Timed("getUpdateProduct")
    @ResponseStatus(HttpStatus.OK)
    public ProductDto updateProduct
    (Principal principal, @PathVariable Long id, @Valid @RequestBody ProductRequest request) {
        return productService.updateProduct(principal, id, request);
    }

    /**
     * Deletes a product.
     *
     * @param principal The principal (typically representing the logged-in user).
     * @param id        The ID of the product to be deleted.
     */
    @DeleteMapping("/delete/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @Timed("getDeleteProduct")
    public void deleteProduct(Principal principal, @PathVariable Long id) {
        productService.deleteProduct(principal, id);
    }

    /**
     * This is the endpoint to add a product to your favorites
     *
     * @param principal The principal (typically representing the logged-in user).
     * @param id ID of the product to be added to favorites.
     */
    @PostMapping("/favorite/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void getFavoriteProduct(Principal principal, @PathVariable Long id){
        productService.getFavorite(principal, id);
    }

    /**
     * This is the endpoint to remove a product from your favorite
     *
     * @param principal The principal (typically representing the logged-in user).
     * @param id ID of the product to be removed from favorites.
     */
    @DeleteMapping("/favorite/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void deleteFavorite(Principal principal, @PathVariable Long id){
        productService.deleteFavorite(principal, id);
    }
}

