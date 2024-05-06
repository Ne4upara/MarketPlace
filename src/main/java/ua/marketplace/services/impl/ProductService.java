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
import ua.marketplace.dto.ImageDto;
import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.dto.Pagination;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.entities.*;
import ua.marketplace.mapper.ProductMapper;
import ua.marketplace.repositoryes.CategoryRepository;
import ua.marketplace.repositoryes.FavoriteRepository;
import ua.marketplace.repositoryes.ProductRepository;
import ua.marketplace.requests.ProductRequest;
import ua.marketplace.services.IProductService;
import ua.marketplace.utils.ErrorMessageHandler;

import java.security.Principal;
import java.util.List;
import java.util.stream.Stream;

/**
 * Implementation of the {@link IProductService} interface for managing product-related operations.
 */
@Service
@RequiredArgsConstructor
public class ProductService implements IProductService {

    private final ProductRepository productRepository;
    private final CategoryRepository categoryRepository;
    private final ImageService imageService;
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
        Page<Product> pageAll = productRepository.findAll(utilsService.getPageRequest(
                pageNumber, pageSize, sortBy, orderBy));
        List<MainPageProductDto> pageAllContent = utilsService.convertProductListToDto(pageAll);

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

        Category byCategoryName = categoryRepository.findByCategoryName(category)
                .orElseThrow(() -> new ResponseStatusException
                        (HttpStatus.NOT_FOUND, String.format(ErrorMessageHandler.INVALID_CATEGORY, category)));

        Pageable pageable = utilsService.getPageRequest(pageNumber, pageSize, sortBy, orderBy);
        Page<Product> pageAll = productRepository.findByCategory(byCategoryName, pageable);
        List<MainPageProductDto> pageAllContent = utilsService.convertProductListToDto(pageAll);

        return new Pagination(pageAll.getNumber(),
                pageAll.getTotalElements(),
                pageAll.getTotalPages(),
                pageAllContent);
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
        return ProductMapper.PRODUCT_INSTANCE.productToProductDto(utilsService.getProductById(id));
    }

    public void incrementViewProduct(HttpSession session, Long id){ //Надо тестировать!!
        String attributeName = "visited_product_" + id;
        if (session.getAttribute(attributeName) == null) {
            productRepository.incrementProductViews(id);
            session.setAttribute(attributeName, "up");
        }
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
        User user = utilsService.getUserByPrincipal(principal);
        Product product = createProduct(request, user);

        return ProductMapper.PRODUCT_INSTANCE.productToProductDto(productRepository.save(product));
    }

    private Product createProduct(ProductRequest request, User user) {

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

        product.setPhotos(imageService.getPhotoLinks(request.productPhotoLink(), product));
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
    public ProductDto updateProduct(Principal principal, Long productId, ProductRequest request) {
        User user = utilsService.getUserByPrincipal(principal);
        Product product = utilsService.getProductById(productId);

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

                    return p;
                })
                .map(productRepository::save)
                .findFirst()
                .orElseThrow(() -> new ResponseStatusException
                        (HttpStatus.BAD_REQUEST, ErrorMessageHandler.FAILED_PRODUCT_UPDATE));


        return ProductMapper.PRODUCT_INSTANCE.productToProductDto(updatedProduct);
    }

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
        User user = utilsService.getUserByPrincipal(principal);
        Product product = utilsService.getProductById(productId);

        if (!isProductCreatedByUser(product, user)) {
            throw new ResponseStatusException
                    (HttpStatus.CONFLICT, ErrorMessageHandler.THIS_NOT_USERS_PRODUCT);
        }

        imageService.deleteFile(product.getPhotos()); //new
        productRepository.delete(product);
    }

    @Override
    public void getFavorite(Principal principal, Long id) {
        User user = utilsService.getUserByPrincipal(principal);
        Product productById = utilsService.getProductById(id);
        validateFavorite(user, productById, "TRUE");
        Favorite favorite = Favorite.builder()
                .user(user)
                .product(productById)
                .build();
        favoriteRepository.save(favorite);
    }

    @Override
    @Transactional //проверить
    public void deleteFavorite(Principal principal, Long id) {
        User userByPrincipal = utilsService.getUserByPrincipal(principal);
        Product productById = utilsService.getProductById(id);
        validateFavorite(userByPrincipal, productById, "FALSE");
        Favorite byUserAndProduct = favoriteRepository.findByUserAndProduct(userByPrincipal, productById);
        userByPrincipal.getFavorites().remove(byUserAndProduct);
        productById.getFavorites().remove(byUserAndProduct);
        favoriteRepository.delete(byUserAndProduct);
    }

    private void validateFavorite(User user, Product product, String exists) {
        if (Boolean.FALSE.equals(favoriteRepository.existsByUserAndProduct(user, product)) && "FALSE".equals(exists)) {
            throw new ResponseStatusException
                    (HttpStatus.BAD_REQUEST, String.format(
                            ErrorMessageHandler.INVALID_FAVORITE, product.getId()));
        }
        if (Boolean.TRUE.equals(favoriteRepository.existsByUserAndProduct(user, product)) && "TRUE".equals(exists)) {
            throw new ResponseStatusException
                    (HttpStatus.BAD_REQUEST, String.format(
                            ErrorMessageHandler.INVALID_FAVORITE, product.getId()));
        }
    }


    public ProductDto test(Principal principal, ProductRequest request, List<MultipartFile> files) {
        User user = utilsService.getUserByPrincipal(principal);
        Product product = testtwo(request, user, files);

        return ProductMapper.PRODUCT_INSTANCE.productToProductDto(productRepository.save(product));
    }

    private Product testtwo(ProductRequest request, User user, List<MultipartFile> files) {

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

        List<String> listPhotoLinks = imageService.upLoadFile(files);

        product.setPhotos(imageService.getPhotoLinks(listPhotoLinks, product));
        return product;
    }
}
