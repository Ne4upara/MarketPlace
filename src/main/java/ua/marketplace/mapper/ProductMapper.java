package ua.marketplace.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import ua.marketplace.dto.MainPageProductDto;
import ua.marketplace.dto.ProductDto;
import ua.marketplace.entities.Product;

import java.math.BigDecimal;

@Mapper(componentModel = "spring")
public interface ProductMapper {
    @Mapping(target = "productRating", expression = "java(getRating(product))")
    ProductDto toDto(Product product);

    @Mapping(target = "productRating", expression = "java(getRating(product))")
    MainPageProductDto toMainPageDto(Product product);

    default int getRating(Product product) {
        if (product.getProductRatingCount() == BigDecimal.ZERO.intValue()) {
            return BigDecimal.ZERO.intValue();
        }
        return product.getProductRating() / product.getProductRatingCount();
    }
}
