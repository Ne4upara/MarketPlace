package ua.marketplace.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;
import ua.marketplace.dto.OrderListDto;
import ua.marketplace.dto.OrderListUserInfoDto;
import ua.marketplace.dto.ProductFromOrderListDto;
import ua.marketplace.entities.OrderList;
import ua.marketplace.entities.Product;

/**
 * Mapper interface for converting between OrderListService entities and DTOs.
 * Uses MapStruct for mapping configurations.
 */
@Mapper(componentModel = "spring")
public interface OrderListMapper {

    OrderListMapper ORDER_LIST_MAPPER_INSTANCE = Mappers.getMapper(OrderListMapper.class);

    /**
     * Converts an OrderListService entity to an OrderListDto.
     *
     * @param orderList The OrderListService entity to convert.
     * @return The corresponding OrderListDto.
     */
    OrderListDto orderListToOrderListDto(OrderList orderList);

    /**
     * Converts a Product entity to a ProductFromOrderListDto.
     *
     * @param product The Product entity to convert.
     * @return The corresponding ProductFromOrderListDto.
     */
    @Mapping(target = "productPhotoLink",
            expression = "java(ProductMapper.PRODUCT_INSTANCE.getMainPagePhotoLink(product))")
    ProductFromOrderListDto productToProductFromOrderListDto(Product product);

    /**
     * Converts an OrderListService entity to an OrderListUserInfoDto.
     * Calculates the count of products in the order list.
     *
     * @param orderList The OrderListService entity to convert.
     * @return The corresponding OrderListUserInfoDto.
     */
    @Mapping(target = "count", expression = "java(getProductCount(orderList))")
    OrderListUserInfoDto orderListToOrderUserInfoDto(OrderList orderList);

    /**
     * Helper method to calculate the count of products in an OrderListService.
     *
     * @param orderList The OrderListService entity.
     * @return The count of products in the order list.
     */
    default int getProductCount(OrderList orderList) {
        return orderList.getProducts().size();
    }
}
