package ua.marketplace.services;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.utils.ErrorMessageHandler;
import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.User;
import ua.marketplace.exception.AppException;
import ua.marketplace.mapper.ProductMapper;
import ua.marketplace.repositoryes.ProductRepository;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.ProductRequest;

import java.math.BigDecimal;
import java.security.Principal;
import java.util.List;
import java.util.stream.Stream;

@Service
@RequiredArgsConstructor
public class ProductService implements IProductService {

    private final ProductRepository productRepository;
    private final UserRepository userRepository;
    private final ProductMapper productMapper;

    @Override
    public List<MainPageProductDto> getAllProductsForMainPage() {
        return convertProductListToDto(productRepository.findAll());
    }
    private List<MainPageProductDto> convertProductListToDto(List<Product> products) {
        return products.stream().map(productMapper::toMainPageDto)
                .toList();
        }

    @Override
    public ProductDto getProductDetails(Long id) {
        return productMapper.toDto(getProductById(id));
    }

    private Product getProductById(Long id) {
        return productRepository.findById(id)
                .orElseThrow(() -> new ResponseStatusException
                        (HttpStatus.NOT_FOUND, ErrorMessageHandler.PRODUCT_NOT_FOUND + id));
    }

    @Override
    public ProductDto saveProduct(Principal principal, ProductRequest request) {
        User user = getUserByPrincipal(principal);
        Product product = createProduct(request, user);

        return productMapper.toDto(productRepository.save(product));
    }

    private User getUserByPrincipal(Principal principal) {
        return userRepository.findByPhoneNumber(principal.getName())
                .orElseThrow(() -> new AppException(ErrorMessageHandler.USER_NOT_AUTHORIZED));
    }

    private Product createProduct(ProductRequest request, User user) {
        return Product
                .builder()
                .productName(request.productName())
                .productPhotoLink(request.productPhotoLink())
                .productPrice(request.productPrice())
                .productDescription(request.productDescription())
                .productCategory(request.productCategory())
                .productType(request.productType())
                .productQuantity(request.productQuantity())
                .owner(user)
                .build();
    }

    @Override
    public ProductDto updateProduct(Principal principal, Long productId, ProductRequest request) {
        User user = getUserByPrincipal(principal);
        Product product = getProductById(productId);

        if (!isProductCreatedByUser(product, user)) {
            throw new AppException(ErrorMessageHandler.NOT_AUTHORIZED);
        }

        Product updatedProduct = Stream.of(product)
                .map(p -> {
                    p.setProductName(request.productName());
                    p.setProductPhotoLink(request.productPhotoLink());
                    p.setProductPrice(request.productPrice());
                    p.setProductDescription(request.productDescription());
                    p.setProductCategory(request.productCategory());
                    p.setProductType(request.productType());
                    p.setProductQuantity(request.productQuantity());
                    return p;
                })
                .map(productRepository::save)
                .findFirst()
                .orElseThrow(() -> new AppException(ErrorMessageHandler.FAILED_PRODUCT_UPDATE));

        return productMapper.toDto(updatedProduct);
    }

    private boolean isProductCreatedByUser(Product product, User user) {
        return product.getOwner().equals(user);
    }

    @Override
    public ProductDto rateProduct(Long productId, int rating) {
        if (!isRatingValid(rating)) {
            throw new AppException(ErrorMessageHandler.PRODUCT_RATING_ERROR);
        }

        Product product = getProductById(productId);

        product.setProductRating(product.getProductRating() + rating);
        product.setProductRatingCount(product.getProductRatingCount() + BigDecimal.ONE.intValue());

        Product saved = productRepository.save(product);
        return productMapper.toDto(saved);
    }

    private boolean isRatingValid(int rating) {
        return rating >= 0 && rating <= 5;
    }

    @Override
    public void deleteProduct(Principal principal, Long productId) {
        User user = getUserByPrincipal(principal);
        Product product = getProductById(productId);

        if (!isProductCreatedByUser(product, user)) {
            throw new AppException(ErrorMessageHandler.DELETING_WITH_NOT_AUTHORIZED_USER);
        }

        productRepository.delete(product);
    }
}
