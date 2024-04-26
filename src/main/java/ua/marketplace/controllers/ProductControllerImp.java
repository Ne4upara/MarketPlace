package ua.marketplace.controllers;

import io.micrometer.core.annotation.Counted;
import io.micrometer.core.annotation.Timed;
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

/**
 * Controller class for managing product-related operations. This class handles HTTP requests for product-related operations, such as retrieving, creating, updating, and deleting products.
 */
@RestController
@RequestMapping("/v1/products")
@RequiredArgsConstructor
public class ProductControllerImp implements IProductController {

    private final ProductService productService; // Dependency injection of ProductService

    /**
     * Retrieves all products for the main page.
     *
     * @param number      Zero-based index of the first product to retrieve (default is 0).
     * @param size        Number of products to retrieve (default is 10).
     * @param sort        Field to sort the products by (default is creationDate, other options are productName, productPrice, id).
     * @param order       Sorting direction (default is DESC, other option is ASC).
     * @return Pagination object containing a list of MainPageProductDto and total elements.
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
     * @param number      Zero-based index of the first product to retrieve (default is 0).
     * @param size        Number of products to retrieve (default is 10).
     * @param sort        Field to sort the products by (default is creationDate, other options are productName, productPrice, id).
     * @param order       Sorting direction (default is DESC, other option is ASC).
     * @param category    Category of the products to retrieve.
     * @return Pagination object containing a list of MainPageProductDto and total elements.
     */
    @GetMapping("/s/view/{category}")
    @ResponseStatus(HttpStatus.OK)
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
    public void deleteProduct(Principal principal, @PathVariable Long id) {
        productService.deleteProduct(principal, id);
    }

    /**
     * Retrieves all products for the user's profile.
     *
     * @param number      Zero-based index of the first product to retrieve (default is 0).
     * @param size        Number of products to retrieve (default is 10).
     * @param sort        Field to sort the products by (default is creationDate, other options are productName, productPrice, id).
     * @param order       Sorting direction (default is ASC, other option is DESC).
     * @param principal   The principal (typically representing the logged-in user).
     * @return Pagination object containing a list of ProductDto and total elements.
     */
    @GetMapping("/view/my-profile/all")
    @ResponseStatus(HttpStatus.OK)
    public Pagination getViewMyProduct(
            @Valid @RequestParam(defaultValue = "0") @PositiveOrZero int number,
            @Valid @RequestParam(defaultValue = "10") @Positive int size,
            @Valid @RequestParam(defaultValue = "creationDate")
            @Pattern(regexp = "creationDate|productName|productPrice|id") String sort,
            @Valid @RequestParam(defaultValue = "ASC") @Pattern(regexp = "ASC|DESC") String order,
            Principal principal){
        return productService.getViewMyProduct(number, size, sort, order, principal);
    }

}

