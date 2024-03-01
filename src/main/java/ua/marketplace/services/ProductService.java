package ua.marketplace.services;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.User;
import ua.marketplace.exception.AppException;
import ua.marketplace.repositoryes.ProductRepository;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.ProductRequest;

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
                .map(product -> MainPageProductDto.builder()
                        .productPhotoLink(product.getProductPhotoLink())
                        .productName(product.getProductName())
                        .productType(product.getProductType())
                        .productPrice(product.getProductPrice())
                        .productRating(getRating(product))
                        .build())
                .toList();
    }

    @Override
    public ProductDto getProductDetails(Long id) throws AppException {
        return convertProductToDto(getProductById(id));
    }

    private ProductDto convertProductToDto(Product product) {
        return ProductDto
                .builder()
                .productPhotoLink(product.getProductPhotoLink())
                .productName(product.getProductName())
                .productType(product.getProductType())
                .productPrice(product.getProductPrice())
                .productDescription(product.getProductDescription())
                .productCategory(product.getProductCategory())
                .productRating(getRating(product))
                .creationDate(product.getCreationDate())
                .productQuantity(product.getProductQuantity())
                .build();
    }

    private int getRating(Product product) {
        if (product.getProductRatingCount() == 0) {
            return 0;
        }

        return product.getProductRating() / product.getProductRatingCount();
    }

    @Override
    public ProductDto saveProduct(Principal principal, ProductRequest request) throws AppException {
        User user = getUserByPrincipal(principal);
        Product product = createProduct(request, user);

        return convertProductToDto(productRepository.save(product));
    }

    private User getUserByPrincipal(Principal principal) throws AppException {
        return userRepository.findByPhoneNumber(principal.getName())
                .orElseThrow(() -> new AppException("User not authorized"));
    }

    private Product createProduct(ProductRequest request, User user) {
        return Product
                .builder()
                .productName(request.getProductName())
                .productPhotoLink(request.getProductPhotoLink())
                .productPrice(request.getProductPrice())
                .productDescription(request.getProductDescription())
                .productCategory(request.getProductCategory())
                .productType(request.getProductType())
                .productQuantity(request.getProductQuantity())
                .owner(user)
                .build();
    }

    @Override
    public ProductDto updateProduct(Principal principal, Long productId, ProductRequest request) throws AppException {
        User user = getUserByPrincipal(principal);
        Product product = getProductById(productId);

        if (!isProductCreatedByUser(product, user)) {
            throw new AppException("You are not authorized to update this product");
        }

        Product updatedProduct = Stream.of(product)
                .map(p -> {
                    p.setProductName(request.getProductName());
                    p.setProductPhotoLink(request.getProductPhotoLink());
                    p.setProductPrice(request.getProductPrice());
                    p.setProductDescription(request.getProductDescription());
                    p.setProductCategory(request.getProductCategory());
                    p.setProductType(request.getProductType());
                    p.setProductQuantity(request.getProductQuantity());
                    return p;
                })
                .map(productRepository::save)
                .findFirst()
                .orElseThrow(() -> new AppException("Failed to update product"));

        return convertProductToDto(updatedProduct);
    }

    private boolean isProductCreatedByUser(Product product, User user) {
        return product.getOwner().equals(user);
    }

    private Product getProductById(Long id) throws AppException {
        return productRepository.findById(id)
                .orElseThrow(() -> new AppException("Product with ID: " + id + " not found "));
    }

    @Override
    public ProductDto rateProduct(Long productId, int rating) throws AppException {
        if(!isRatingValid(rating)) {
            throw new AppException ("Rating must be between 0 and 5");
        }

        Product product = getProductById(productId);

        product.setProductRating(product.getProductRating() + rating);
        product.setProductRatingCount(product.getProductRatingCount() + 1);

        Product saved = productRepository.save(product);
        return convertProductToDto(saved);
    }

    private boolean isRatingValid(int rating) {
        return rating >= 0 && rating <= 5;
    }

    @Override
    public void deleteProduct(Principal principal, Long productId) throws AppException {
        User user = getUserByPrincipal(principal);
        Product product = getProductById(productId);

        if (!isProductCreatedByUser(product, user)) {
            throw new AppException("You are not authorized to delete this product");
        }

        productRepository.delete(product);
    }
}
