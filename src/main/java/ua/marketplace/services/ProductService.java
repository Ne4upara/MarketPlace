package ua.marketplace.services;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.dto.Pagination;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.User;
import ua.marketplace.mapper.ProductMapper;
import ua.marketplace.repositoryes.ProductRepository;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.ProductRequest;
import ua.marketplace.utils.ErrorMessageHandler;

import java.math.BigDecimal;
import java.security.Principal;
import java.util.List;
import java.util.stream.Stream;

/**
 * Service class for managing product-related operations.
 */
@Service
@RequiredArgsConstructor
public class ProductService implements IProductService {

    private static final int MIN_RATE = 0;
    private static final int MAX_RATE = 5;
    private final ProductRepository productRepository;
    private final UserRepository userRepository;

    /**
     * Retrieves details of all products for the main page, paginated and sorted.
     *
     * @param pageNumber The page number to retrieve.
     * @param pageSize   The number of products per page.
     * @param sortBy     The field to sort by.
     * @param orderBy    The order direction ('ASC' for ascending, 'DESC' for descending).
     * @return Pagination object containing the requested products.
     */
    @Override
    public Pagination getAllProductsForMainPage(int pageNumber, int pageSize, String sortBy, String orderBy) {
        Page<Product> pageAll = productRepository.findAll(getPageRequest(
                pageNumber, pageSize, sortBy,orderBy));
        List<MainPageProductDto> pageAllContent = convertProductListToDto(pageAll);

        return new Pagination(pageAll.getNumber(),
                pageAll.getTotalElements(),
                pageAll.getTotalPages(),
                pageAllContent);
    }

    /**
     * Retrieves a Pageable object for pagination and sorting.
     *
     * @param num     The page number.
     * @param size    The number of items per page.
     * @param sortBy  The field to sort by.
     * @param orderBy The order direction ('ASC' for ascending, 'DESC' for descending).
     * @return Pageable object for pagination and sorting.
     */
    private Pageable getPageRequest(int num, int size, String sortBy, String orderBy) {
        return PageRequest.of(num, size, isSort(sortBy, orderBy));
    }

    /**
     * Determines the sort order based on the provided parameters.
     *
     * @param sortBy  The field to sort by.
     * @param orderBy The order direction ('ASC' for ascending, 'DESC' for descending).
     * @return Sort object representing the sorting criteria.
     */
    private Sort isSort(String sortBy, String orderBy) {
        if ("ASC".equals(orderBy)) return Sort.by(sortBy).ascending();
        return Sort.by(sortBy).descending();
    }

    /**
     * Converts a list of Product entities to MainPageProductDto.
     *
     * @param products The list of Product entities to be converted.
     * @return List of MainPageProductDto containing mapped product details.
     */
    private List<MainPageProductDto> convertProductListToDto(Page<Product> products) {
        return products.stream().map(ProductMapper.INSTANCE::productToMainPageDto)
                .toList();
    }

    /**
     * Retrieves details of a product by its ID.
     *
     * @param id The ID of the product to retrieve.
     * @return ProductDto containing details of the product.
     * @throws ResponseStatusException if the product is not found.
     */
    @Override
    public ProductDto getProductDetails(Long id) {
        return ProductMapper.INSTANCE.productToProductDto(getProductById(id));
    }

    /**
     * Retrieves a Product entity by its ID.
     *
     * @param id The ID of the product to retrieve.
     * @return The retrieved Product entity.
     * @throws ResponseStatusException if the product is not found.
     */
    private Product getProductById(Long id) {
        return productRepository.findById(id)
                .orElseThrow(() -> new ResponseStatusException
                        (HttpStatus.NOT_FOUND, ErrorMessageHandler.PRODUCT_NOT_FOUND + id));
    }

    /**
     * Saves a new product.
     *
     * @param principal The principal (typically representing the logged-in user).
     * @param request   ProductRequest containing details of the product to be saved.
     * @return ProductDto containing details of the newly saved product.
     */
    @Override
    public ProductDto saveProduct(Principal principal, ProductRequest request) {
        User user = getUserByPrincipal(principal);
        Product product = createProduct(request, user);

        return ProductMapper.INSTANCE.productToProductDto(productRepository.save(product));
    }

    /**
     * Retrieves the user associated with the given principal.
     *
     * @param principal The principal object representing the logged-in user.
     * @return The User entity associated with the principal.
     * @throws ResponseStatusException if the user is not found or not authorized.
     */
    private User getUserByPrincipal(Principal principal) {
        return userRepository.findByPhoneNumber(principal.getName())
                .orElseThrow(() -> new ResponseStatusException
                        (HttpStatus.FORBIDDEN, ErrorMessageHandler.USER_NOT_AUTHORIZED));
    }

    /**
     * Creates a new Product entity based on the provided request and user.
     *
     * @param request The request containing product details.
     * @param user    The owner of the product.
     * @return The newly created Product entity.
     */
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

    /**
     * Updates an existing product.
     *
     * @param principal The principal (typically representing the logged-in user).
     * @param productId The ID of the product to be updated.
     * @param request   ProductRequest containing details to update the product.
     * @return ProductDto containing details of the updated product.
     * @throws ResponseStatusException if the product is not found or not authorized.
     */
    @Override
    public ProductDto updateProduct(Principal principal, Long productId, ProductRequest request) {
        User user = getUserByPrincipal(principal);
        Product product = getProductById(productId);

        if (!isProductCreatedByUser(product, user)) {
            throw new ResponseStatusException(HttpStatus.CONFLICT, ErrorMessageHandler.THIS_NOT_USERS_PRODUCT);
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
                .orElseThrow(() -> new ResponseStatusException
                        (HttpStatus.BAD_REQUEST, ErrorMessageHandler.FAILED_PRODUCT_UPDATE));

        return ProductMapper.INSTANCE.productToProductDto(updatedProduct);
    }

    /**
     * Checks if the given user is the owner of the product.
     *
     * @param product The product to check ownership for.
     * @param user    The user to compare ownership with.
     * @return true if the user is the owner of the product, false otherwise.
     */
    private boolean isProductCreatedByUser(Product product, User user) {
        return product.getOwner().equals(user);
    }

    /**
     * Rates a product.
     *
     * @param productId The ID of the product to rate.
     * @param rating    The rating to assign to the product.
     * @return ProductDto containing details of the rated product.
     * @throws ResponseStatusException if the rating is invalid or the product is not found.
     */
    @Override
    public ProductDto rateProduct(Principal principal, Long productId, int rating) {

        getUserByPrincipal(principal);

        if (!isRatingValid(rating)) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, ErrorMessageHandler.PRODUCT_RATING_ERROR);
        }

        Product product = getProductById(productId);

        product.setProductRating(product.getProductRating() + rating);
        product.setProductRatingCount(product.getProductRatingCount() + BigDecimal.ONE.intValue());

        Product saved = productRepository.save(product);
        return ProductMapper.INSTANCE.productToProductDto(saved);
    }

    /**
     * Checks if the given rating value is valid.
     *
     * @param rating The rating value to check.
     * @return true if the rating is valid, false otherwise.
     */
    private boolean isRatingValid(int rating) {
        return rating >= MIN_RATE && rating <= MAX_RATE;
    }

    /**
     * Deletes a product.
     *
     * @param principal The principal (typically representing the logged-in user).
     * @param productId The ID of the product to be deleted.
     * @throws ResponseStatusException if the product is not found or not authorized.
     */
    @Override
    public void deleteProduct(Principal principal, Long productId) {
        User user = getUserByPrincipal(principal);
        Product product = getProductById(productId);

        if (!isProductCreatedByUser(product, user)) {
            throw new ResponseStatusException
                    (HttpStatus.CONFLICT, ErrorMessageHandler.THIS_NOT_USERS_PRODUCT);
        }

        productRepository.delete(product);
    }
}
