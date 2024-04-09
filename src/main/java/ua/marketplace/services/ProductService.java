package ua.marketplace.services;

import jakarta.transaction.Transactional;
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
import ua.marketplace.entities.Category;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.User;
import ua.marketplace.mapper.ProductMapper;
import ua.marketplace.repositoryes.CategoryRepository;
import ua.marketplace.repositoryes.ProductRepository;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.ProductRequest;
import ua.marketplace.utils.ErrorMessageHandler;

import java.security.Principal;
import java.util.List;
import java.util.Locale;
import java.util.stream.Stream;

/**
 * Service class for managing product-related operations.
 */
@Service
@RequiredArgsConstructor
public class ProductService implements IProductService {

    private final ProductRepository productRepository;
    private final UserRepository userRepository;
    private final CategoryRepository categoryRepository;
    private final ImageService imageService;

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
                pageNumber, pageSize, sortBy, orderBy));
        List<MainPageProductDto> pageAllContent = convertProductListToDto(pageAll);

        return new Pagination(pageAll.getNumber(),
                pageAll.getTotalElements(),
                pageAll.getTotalPages(),
                pageAllContent);
    }

    /**
     * Retrieves products paginated filtered by category.
     *
     * @param pageNumber The page number to retrieve.
     * @param pageSize   The number of products per page.
     * @param sortBy     The field to sort the products by (e.g., "creationDate", "productName", "productPrice", "id").
     * @param orderBy    The sorting order ("ASC" for ascending, "DESC" for descending).
     * @param category   The category by which to filter the products.
     * @return Pagination object containing the paginated list of products for the main page filtered by category.
     */
    @Override
    public Pagination getAllProductsByCategory
    (int pageNumber, int pageSize, String sortBy, String orderBy, String category) {

        Category byCategoryName = categoryRepository.findByCategoryName(category.toUpperCase(Locale.ENGLISH))
                .orElseThrow(() -> new ResponseStatusException
                        (HttpStatus.NOT_FOUND, String.format(ErrorMessageHandler.INVALID_CATEGORY, category)));


        Pageable pageable = getPageRequest(pageNumber, pageSize, sortBy, orderBy);
        Page<Product> pageAll = productRepository.findByCategory(byCategoryName, pageable);
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
        return products.stream().map(ProductMapper.PRODUCT_INSTANCE::productToMainPageDto)
                .toList();
    }

    /**
     * Retrieves details of a product by its ID.
     *
     * @param id The ID of the product to retrieve.
     * @return ProductDto containing details of the product.
     * @throws ResponseStatusException if the product is not found.
     */

    @Transactional
    @Override
    public ProductDto getProductDetails(Long id) {
        productRepository.incrementProductViews(id); //Треба протестити
        return ProductMapper.PRODUCT_INSTANCE.productToProductDto(getProductById(id));
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

        return ProductMapper.PRODUCT_INSTANCE.productToProductDto(productRepository.save(product));
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
                        (HttpStatus.UNAUTHORIZED, ErrorMessageHandler.USER_NOT_AUTHORIZED));
    }

    /**
     * Creates a new Product entity based on the provided request and user.
     *
     * @param request The request containing product details.
     * @param user    The owner of the product.
     * @return The newly created Product entity.
     */
    private Product createProduct(ProductRequest request, User user) {

        Product product = Product
                .builder()
                .productName(request.productName())
                .productPrice(request.productPrice())
                .productDescription(request.productDescription())
                .category(getCategory(request.productCategory()))
                .productType(request.productType())
                .productQuantity(request.productQuantity())
                .owner(user)
                .sellerName(request.sellerName())
                .sellerPhoneNumber(request.sellerPhoneNumber())
                .sellerEmail(request.sellerEmail())
                .location(request.location())
                .build();

        product.setPhotos(imageService.getPhotoLinks(request.productPhotoLink(), product));
        return product;
    }

    private ua.marketplace.entities.Category getCategory(String categoryName){
        validateCategoryNotExist(categoryName);
        return categoryRepository.findByCategoryName(categoryName)
                .orElseThrow(() -> new ResponseStatusException
                (HttpStatus.NOT_FOUND, String.format(ErrorMessageHandler.INVALID_CATEGORY, categoryName)));
    }

    private void validateCategoryNotExist(String categoryName) {
        if (Boolean.FALSE.equals(categoryRepository.existsByCategoryName(categoryName))) {
            throw new ResponseStatusException
                    (HttpStatus.CONFLICT, String.format(ErrorMessageHandler.INVALID_CATEGORY, categoryName));
        }
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
        imageService.deleteExcessPhotos(request.productPhotoLink().size(), product);
        Product updatedProduct = Stream.of(product)
                .map(p -> {
                    p.setProductName(request.productName());
                    p.setPhotos(imageService.getUpdateLinks(request.productPhotoLink(), p));
                    p.setProductPrice(request.productPrice());
                    p.setProductDescription(request.productDescription());
                    p.setCategory(getCategory(request.productCategory()));
                    p.setProductType(request.productType());
                    p.setProductQuantity(request.productQuantity());
                    return p;
                })
                .map(productRepository::save)
                .findFirst()
                .orElseThrow(() -> new ResponseStatusException
                        (HttpStatus.BAD_REQUEST, ErrorMessageHandler.FAILED_PRODUCT_UPDATE));


        return ProductMapper.PRODUCT_INSTANCE.productToProductDto(updatedProduct);
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
