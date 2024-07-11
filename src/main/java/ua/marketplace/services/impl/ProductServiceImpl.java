package ua.marketplace.services.impl;

import jakarta.servlet.http.HttpSession;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.dto.Pagination;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.entities.*;
import ua.marketplace.mapper.ProductMapper;
import ua.marketplace.repositoryes.CategoryRepository;
import ua.marketplace.repositoryes.FavoriteRepository;
import ua.marketplace.repositoryes.ProductRepository;
import ua.marketplace.requests.ProductRequest;
import ua.marketplace.services.ProductService;
import ua.marketplace.utils.ErrorMessageHandler;

import java.security.Principal;
import java.util.List;

/**
 * Implementation of the {@link ua.marketplace.services.ProductService} interface for managing product-related operations.
 */
@Service
@RequiredArgsConstructor
public class ProductServiceImpl implements ProductService {

    private final ProductRepository productRepository;
    private final CategoryRepository categoryRepository;
    private final ProductPhotoServiceImpl imageService;
    private final FavoriteRepository favoriteRepository;
    private final UtilsService utilsService;

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
        Page<Product> pageAll = createPageAll(pageNumber, pageSize, sortBy, orderBy);
        List<MainPageProductDto> pageAllContent = utilsService.convertProductListToDto(pageAll);
        return createPagination(pageAll, pageAllContent);
    }

    private Page<Product> createPageAll(int pageNumber, int pageSize, String sortBy, String orderBy) {
        return productRepository.findAll(createPageable(pageNumber, pageSize, sortBy, orderBy));
    }

    private Pageable createPageable(int pageNumber, int pageSize, String sortBy, String orderBy) {
        return  utilsService.getPageRequest(pageNumber, pageSize, sortBy, orderBy);
    }

    private Pagination createPagination (Page<Product> page, List<MainPageProductDto> content) {
        return new Pagination(page.getNumber(),
                page.getTotalElements(),
                page.getTotalPages(),
                content);
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
        Category byCategoryName = getCategoryByName(category);
        Pageable pageable = createPageable(pageNumber, pageSize, sortBy, orderBy);
        Page<Product> pageByCategory = createPageByCategory(pageable,byCategoryName);
        List<MainPageProductDto> pageContent = utilsService.convertProductListToDto(pageByCategory);
        return createPagination(pageByCategory, pageContent);
    }

    private Category getCategoryByName(String category) {
        return categoryRepository.findByCategoryName(category)
                .orElseThrow(() -> new ResponseStatusException
                        (HttpStatus.NOT_FOUND, String.format(ErrorMessageHandler.INVALID_CATEGORY, category)));
    }

    private Page<Product> createPageByCategory(Pageable pageable, Category category) {
        return productRepository.findByCategory(category, pageable);
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
//        productRepository.incrementProductViews(id); //Треба протестити
        Product productById = utilsService.getProductById(id);
        return convertToProductDto(productById);
    }

    private ProductDto convertToProductDto(Product product) {
        return ProductMapper.PRODUCT_INSTANCE.productToProductDto(product);
    }

    public void incrementCountOfViewInProduct(HttpSession session, Long id){ //Надо тестировать!!
        String attributeName = "visited_product_" + id;
        if (session.getAttribute(attributeName) == null) {
            incrementProductViewById(id);
            markProductAsVisited(session, attributeName);
        }
    }

    private void incrementProductViewById(Long id) {
        productRepository.incrementProductViews(id);
    }

    private void markProductAsVisited(HttpSession session, String attributeName) {
        session.setAttribute(attributeName, "up");
    }

    /**
     * Saves a new product.
     *
     * @param principal The principal (typically representing the logged-in user).
     * @param request   ProductRequest containing details of the product to be saved.
     * @return ProductDto containing details of the newly saved product.
     */
    @Override
    public ProductDto saveProduct(Principal principal, ProductRequest request,List<MultipartFile> files) {
        User user = utilsService.getUserByPrincipal(principal);
        Product product = createProduct(request, user, files);
        Product savedProduct = productRepository.save(product);
        return convertToProductDto(savedProduct);
    }

    private Product createProduct(ProductRequest request, User user, List<MultipartFile> files) {
        Product product = Product
                .builder()
                .productName(request.productName())
                .productPrice(request.productPrice())
                .productDescription(request.productDescription())
                .category(getCategory(request.productCategory()))
                .productType(request.productType())
                .owner(user)
                .sellerName(request.sellerName())
                .sellerPhoneNumber(request.sellerPhoneNumber())
                .sellerEmail(request.sellerEmail())
                .location(request.location())
                .build();
        product.setPhotos(imageService.getPhotoLinks(files, product));
        return product;
    }

    private Category getCategory(String categoryName) {
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
    public ProductDto updateProduct(Principal principal, Long productId,
                                    ProductRequest request, List<MultipartFile> files) {
        User user = utilsService.getUserByPrincipal(principal);
        Product product = utilsService.getProductById(productId);
        checkUserOwnership(product, user);
        updateProductDetails(product, request, files);
        Product updatedProduct = productRepository.save(product);
        return convertToProductDto(updatedProduct);
    }

    private void checkUserOwnership(Product product, User user) {
        if (!isProductCreatedByUser(product, user)) {
            throw new ResponseStatusException(HttpStatus.CONFLICT, ErrorMessageHandler.THIS_NOT_USERS_PRODUCT);
        }
    }

    private boolean isProductCreatedByUser(Product product, User user) {
        return product.getOwner().equals(user);
    }

    private void updateProductDetails(Product product, ProductRequest request, List<MultipartFile> files) {
        product.setProductName(request.productName());
        product.setPhotos(imageService.getUpdateLinks(files, product));
        product.setProductPrice(request.productPrice());
        product.setProductDescription(request.productDescription());
        product.setCategory(getCategory(request.productCategory()));
        product.setProductType(request.productType());
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
        User user = utilsService.getUserByPrincipal(principal);
        Product product = utilsService.getProductById(productId);
        checkUserOwnership(product, user);
        deleteProductPhoto(product); //new
        productRepository.delete(product);
    }

    private void deleteProductPhoto(Product product) {
        imageService.deleteFiles(product.getPhotos());
    }

    @Override
    public void addProductToFavorite(Principal principal, Long id) {
        User user = utilsService.getUserByPrincipal(principal);
        Product productById = utilsService.getProductById(id);
        validateFavorite(user, productById, "TRUE");
        saveFavorite(user, productById);
    }

    private void saveFavorite(User user, Product product) {
        Favorite favorite = Favorite.builder()
                .user(user)
                .product(product)
                .build();
        favoriteRepository.save(favorite);
    }

    @Override
    @Transactional //проверить
    public void deleteProductFromFavorite(Principal principal, Long id) {
        User user = utilsService.getUserByPrincipal(principal);
        Product product = utilsService.getProductById(id);
        validateFavorite(user, product, "FALSE");
        Favorite byUserAndProduct = findFavoriteByUserAndProduct(user, product);
        removeFavoriteAssociationsAndDelete(byUserAndProduct, user, product);
    }

    private Favorite findFavoriteByUserAndProduct(User user, Product product) {
        return favoriteRepository.findByUserAndProduct(user, product);
    }

    private void  removeFavoriteAssociationsAndDelete(Favorite favorite, User user, Product product) {
        user.getFavorites().remove(favorite);
        product.getFavorites().remove(favorite);
        favoriteRepository.delete(favorite);
    }

    private void validateFavorite(User user, Product product, String expectedStatus) {
        boolean favoriteExists = favoriteRepository.existsByUserAndProduct(user, product);

        if ("FALSE".equals(expectedStatus) && !favoriteExists) {
            throw new ResponseStatusException(
                    HttpStatus.BAD_REQUEST,
                    String.format(ErrorMessageHandler.INVALID_FAVORITE, product.getId()));
        }

        if ("TRUE".equals(expectedStatus) && favoriteExists) {
            throw new ResponseStatusException(
                    HttpStatus.BAD_REQUEST,
                    String.format(ErrorMessageHandler.INVALID_FAVORITE, product.getId()));
        }
    }
}
