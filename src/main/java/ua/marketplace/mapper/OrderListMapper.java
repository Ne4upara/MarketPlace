package ua.marketplace.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;
import ua.marketplace.dto.OrderListDto;
import ua.marketplace.dto.OrderListForMainPageDto;
import ua.marketplace.dto.ProductFromOrderListDto;
import ua.marketplace.entities.OrderList;
import ua.marketplace.entities.Product;

/**
 * Mapper interface for converting between OrderList entities and DTOs.
 * Uses MapStruct for mapping configurations.
 */
@Mapper(componentModel = "spring")
public interface OrderListMapper {

    OrderListMapper ORDER_LIST_MAPPER_INSTANCE = Mappers.getMapper(OrderListMapper.class);

    /**
     * Converts an OrderList entity to an OrderListDto.
     *
     * @param orderList The OrderList entity to convert.
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
     * Converts an OrderList entity to an OrderListForMainPageDto.
     * Calculates the count of products in the order list.
     *
     * @param orderList The OrderList entity to convert.
     * @return The corresponding OrderListForMainPageDto.
     */
    @Mapping(target = "count", expression = "java(getProductCount(orderList))")
    OrderListForMainPageDto orderListToOrderListForMainPageDto(OrderList orderList);

    /**
     * Helper method to calculate the count of products in an OrderList.
     *
     * @param orderList The OrderList entity.
     * @return The count of products in the order list.
     */
    default int getProductCount(OrderList orderList) {
        return orderList.getProducts().size();
    }
}
