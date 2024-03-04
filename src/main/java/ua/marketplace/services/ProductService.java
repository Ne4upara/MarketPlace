package ua.marketplace.services;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import ua.marketplace.constants.ErrorMessage;
import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.User;
import ua.marketplace.exception.AppException;
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

    @Override
    public List<MainPageProductDto> getAllProductsForMainPage() {
        return convertProductListToDto(productRepository.findAll());
    }

    private List<MainPageProductDto> convertProductListToDto(List<Product> products) {
        return products.stream()
                .map(product -> new MainPageProductDto(
                        product.getProductPhotoLink(),
                        product.getProductName(),
                        product.getProductType(),
                        product.getProductPrice(),
                        getRating(product)
                ))
                .toList();
    }

    @Override
    public ProductDto getProductDetails(Long id) {
        return convertProductToDto(getProductById(id));
    }

    private ProductDto convertProductToDto(Product product) {
        return new ProductDto(
                product.getProductName(),
                product.getProductPhotoLink(),
                product.getProductPrice(),
                product.getProductDescription(),
                product.getProductCategory(),
                product.getProductType(),
                product.getCreationDate(),
                getRating(product),
                product.getProductQuantity()
        );
    }

    private int getRating(Product product) {
        if (product.getProductRatingCount() == BigDecimal.ZERO.intValue()) {
            return BigDecimal.ZERO.intValue();
        }

        return product.getProductRating() / product.getProductRatingCount();
    }

    private Product getProductById(Long id) {
        return productRepository.findById(id)
                .orElseThrow(() -> new AppException(ErrorMessage.PRODUCT_NOT_FOUND + id));
    }

    @Override
    public ProductDto saveProduct(Principal principal, ProductRequest request) {
        User user = getUserByPrincipal(principal);
        Product product = createProduct(request, user);

        return convertProductToDto(productRepository.save(product));
    }

    private User getUserByPrincipal(Principal principal) {
        return userRepository.findByPhoneNumber(principal.getName())
                .orElseThrow(() -> new AppException(ErrorMessage.USER_NOT_AUTHORIZED));
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
            throw new AppException("You are not authorized to update this product");
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
                .orElseThrow(() -> new AppException(ErrorMessage.FAILED_PRODUCT_UPDATE));

        return convertProductToDto(updatedProduct);
    }

    private boolean isProductCreatedByUser(Product product, User user) {
        return product.getOwner().equals(user);
    }

    @Override
    public ProductDto rateProduct(Long productId, int rating) {
        if (!isRatingValid(rating)) {
            throw new AppException(ErrorMessage.PRODUCT_RATING_ERROR);
        }

        Product product = getProductById(productId);

        product.setProductRating(product.getProductRating() + rating);
        product.setProductRatingCount(product.getProductRatingCount() + BigDecimal.ONE.intValue());

        Product saved = productRepository.save(product);
        return convertProductToDto(saved);
    }

    private boolean isRatingValid(int rating) {
        return rating >= 0 && rating <= 5;
    }

    @Override
    public void deleteProduct(Principal principal, Long productId) {
        User user = getUserByPrincipal(principal);
        Product product = getProductById(productId);

        if (!isProductCreatedByUser(product, user)) {
            throw new AppException(ErrorMessage.DELETING_WITH_NOT_AUTHORIZED_USER);
        }

        productRepository.delete(product);
    }
}
