package ua.marketplace.controllers;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.requests.ProductRequest;
import ua.marketplace.services.ProductService;

import java.security.Principal;
import java.util.List;

@RestController
@RequestMapping("/api/v1/products")
@RequiredArgsConstructor
public class ProductControllerImp implements IProductController {

    private final ProductService productService;

    @GetMapping("/list")
    public List<MainPageProductDto> getAllProductsForMainPage() {
        return productService.getAllProductsForMainPage();
    }

    @GetMapping("/details/{id}")
    public ProductDto getProductDetailsById(@PathVariable Long id) {
        return productService.getProductDetails(id);
    }

    @PostMapping("/create")
    @ResponseStatus(HttpStatus.CREATED)
    public ProductDto createProduct(Principal principal, @Valid @RequestBody ProductRequest request) {
        return productService.saveProduct(principal, request);
    }

    @PostMapping("/update/{id}")
    public ProductDto updateProduct
            (Principal principal, @PathVariable Long id, @Valid @RequestBody ProductRequest request) {
        return productService.updateProduct(principal, id, request);
    }

    @PostMapping("/{productId}/rate/{rating}")
    public ProductDto rateProduct(@PathVariable Long productId, @PathVariable int rating) {
        return productService.rateProduct(productId, rating);
    }

    @PostMapping("/delete/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void deleteProduct(Principal principal, @PathVariable Long id) {
        productService.deleteProduct(principal, id);
    }
}
