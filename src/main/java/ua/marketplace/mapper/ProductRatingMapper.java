package ua.marketplace.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;
import ua.marketplace.dto.ProductRatingDto;
import ua.marketplace.entities.ProductRating;

/**
 * Mapper interface for mapping ProductRating entities to ProductRatingDto objects.
 */
//TODO
@Mapper(componentModel = "spring")
public interface ProductRatingMapper {

    /**
     * Singleton instance of the ProductRatingMapper interface.
     */
    ProductRatingMapper PRODUCT_RATING_INSTANCE = Mappers.getMapper(ProductRatingMapper.class);

    /**
     * Maps a ProductRating entity to a ProductRatingDto object.
     *
     * @param productRating The ProductRating entity to map.
     * @return ProductRatingDto object mapped from the ProductRating entity.
     */
    @Mapping(target = "productId", expression = "java(getProductId(productRating))")
    @Mapping(target = "username", expression = "java(getUsername(productRating))")
    ProductRatingDto productRatingToProductRatingDTO(ProductRating productRating);

    /**
     * Retrieves the product ID from the ProductRating entity.
     *
     * @param productRating The ProductRating entity.
     * @return The ID of the product associated with the rating.
     */
    default Long getProductId(ProductRating productRating) {
        return productRating.getProduct().getId();
    }

    /**
     * Retrieves the username from the User entity associated with the ProductRating.
     *
     * @param productRating The ProductRating entity.
     * @return The username of the user who rated the product.
     */
    default String getUsername(ProductRating productRating) {
        return productRating.getUser().getFirstName();

    }
}
