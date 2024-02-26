package ua.marketplace.services;

import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.exception.AppException;
import ua.marketplace.requests.ProductRequest;

import java.security.Principal;
import java.util.List;

public interface IProductService {

    List<MainPageProductDto> getAllProductsForMainPage();
    ProductDto getProductDetails(Long id) throws AppException;
    ProductDto saveProduct(Principal principal, ProductRequest request) throws AppException;

    ProductDto updateProduct(Principal principal, ProductRequest request, Long id) throws AppException;

    void deleteProduct(Principal principal, Long productId) throws AppException;
}
