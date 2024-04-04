package ua.marketplace.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;
import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.ProductPhoto;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Mapper interface for converting Product entities to DTOs.
 * Uses MapStruct for mapping configurations.
 */
@Mapper(componentModel = "spring")
public interface ProductMapper {

    ProductMapper INSTANCE = Mappers.getMapper(ProductMapper.class);

    /**
     * Converts a Product entity to a ProductDto.
     *
     * @param product The Product entity to be converted.
     * @return ProductDto containing the mapped attributes.
     */

    @Mapping(target = "productRating", expression = "java(getRating(product))")
    @Mapping(target = "productCategory", source = "product.category.categoryName")
    @Mapping(target = "productPhotoLink", expression = "java(getAllPhotoLink(product))")
    ProductDto productToProductDto(Product product);

    /**
     * Converts a Product entity to a MainPageProductDto.
     *
     * @param product The Product entity to be converted.
     * @return MainPageProductDto containing the mapped attributes for the main page.
     */
    @Mapping(target = "productRating", expression = "java(getRating(product))")
    @Mapping(target = "productPhotoLink", expression = "java(getMainPagePhotoLink(product))")
    MainPageProductDto productToMainPageDto(Product product);

    /**
     * Calculates the average rating for a given Product entity.
     *
     * @param product The Product entity for which the rating is calculated.
     * @return The calculated average rating.
     */
    default int getRating(Product product) {
//        if (product.getProductRatingCount() == BigDecimal.ZERO.intValue()) {
            return BigDecimal.ZERO.intValue();
        }
//        return product.getProductRating() / product.getProductRatingCount();
//    }

    default String getMainPagePhotoLink(Product product) {
        List<ProductPhoto> photos = product.getPhotos();
        if (photos != null && !photos.isEmpty()) {
            for (ProductPhoto photo : photos) {
                if (photo.isMainPage()) {
                    return photo.getPhotoLink();
                }
            }
        }
        return null;
    }

    default List<String> getAllPhotoLink(Product product) {
        return product.getPhotos().stream()
                .map(ProductPhoto::getPhotoLink)
                .collect(Collectors.toList());
    }
}
