package ua.marketplace.services;

import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.requests.ProductRequest;

import java.security.Principal;
import java.util.List;

public interface IProductService {

    List<MainPageProductDto> getAllProductsForMainPage();

    ProductDto getProductDetails(Long id);

    ProductDto saveProduct(Principal principal, ProductRequest request);

    ProductDto updateProduct(Principal principal, Long productId, ProductRequest request);

    ProductDto rateProduct(Long productId, int rating);

    void deleteProduct(Principal principal, Long productId);
}
