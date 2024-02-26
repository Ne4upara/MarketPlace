package ua.marketplace.controllers;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.exception.AppException;
import ua.marketplace.requests.ProductRequest;
import ua.marketplace.services.ProductService;

import java.security.Principal;
import java.util.List;

@RestController
@RequestMapping("/api/v1/product")
@RequiredArgsConstructor
public class ProductController {

    private final ProductService productService;

    @GetMapping("/list")
    public ResponseEntity<List<MainPageProductDto>> getAllProductsForMainPage() {
        return ResponseEntity.status(HttpStatus.OK).body(productService.getAllProductsForMainPage());
    }

    @GetMapping("/details/{id}")
    public ResponseEntity<ProductDto> getProductDetailsById(@PathVariable Long id) throws AppException {
        return ResponseEntity.status(HttpStatus.OK).body(productService.getProductDetails(id));
    }

    @PostMapping("/create")
    public ResponseEntity<ProductDto> createProduct (Principal principal, @RequestBody ProductRequest request) throws AppException {
        return ResponseEntity.status(HttpStatus.CREATED).body(productService.saveProduct(principal,request));
    }

    @PostMapping("/delete/{id}")
    public ResponseEntity<String> deleteProduct(Principal principal, @PathVariable Long id) throws AppException {
        productService.deleteProduct(principal,id);
        return ResponseEntity.status(HttpStatus.OK).body("Successfully deleting");
    }
}
