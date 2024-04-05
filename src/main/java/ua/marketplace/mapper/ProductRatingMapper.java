package ua.marketplace.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;
import ua.marketplace.dto.ProductRatingDto;
import ua.marketplace.entities.ProductRating;

@Mapper(componentModel = "spring")
public interface ProductRatingMapper {

    ProductRatingMapper PRODUCT_RATING_INSTANCE = Mappers.getMapper(ProductRatingMapper.class);

    @Mapping(target = "productId", expression = "java(getProductId(productRating))")
    @Mapping(target = "username", expression = "java(getUsername(productRating))")
    ProductRatingDto productRatingToProductRatingDTO(ProductRating productRating);

    default Long getProductId(ProductRating productRating) {
        return productRating.getProduct().getId();
    }

    default String getUsername(ProductRating productRating) {
        return productRating.getUser().getFirstName();
    }
}
