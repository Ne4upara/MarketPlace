package ua.marketplace.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;
import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.entities.Product;

import java.math.BigDecimal;

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
    ProductDto productToProductDto(Product product);

    /**
     * Converts a Product entity to a MainPageProductDto.
     *
     * @param product The Product entity to be converted.
     * @return MainPageProductDto containing the mapped attributes for the main page.
     */
    @Mapping(target = "productRating", expression = "java(getRating(product))")
    MainPageProductDto toMainPageDto(Product product);

    /**
     * Calculates the average rating for a given Product entity.
     *
     * @param product The Product entity for which the rating is calculated.
     * @return The calculated average rating.
     */
    default int getRating(Product product) {
        if (product.getProductRatingCount() == BigDecimal.ZERO.intValue()) {
            return BigDecimal.ZERO.intValue();
        }
        return product.getProductRating() / product.getProductRatingCount();
    }
}
