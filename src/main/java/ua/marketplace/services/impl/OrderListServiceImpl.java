package ua.marketplace.services.impl;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.dto.OrderListDto;
import ua.marketplace.dto.OrderListUserInfoDto;
import ua.marketplace.entities.OrderList;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.User;
import ua.marketplace.mapper.OrderListMapper;
import ua.marketplace.repositoryes.OrderListRepository;
import ua.marketplace.services.OrderListService;
import ua.marketplace.utils.ErrorMessageHandler;

import java.math.BigDecimal;
import java.security.Principal;
import java.util.ArrayList;

/**
 * Service class for managing order lists.
 * Implements the OrderListService interface.
 */
@Service
@RequiredArgsConstructor
public class OrderListServiceImpl implements OrderListService {

    private final OrderListRepository orderListRepository;
    private final UtilsService utilsService;

    /**
     * Retrieves the order list associated with the authenticated user.
     *
     * @param principal The principal representing the authenticated user.
     * @return The DTO representing the order list.
     */
    @Override
    public OrderListDto viewOrderList(Principal principal) {
        return OrderListMapper.ORDER_LIST_MAPPER_INSTANCE.orderListToOrderListDto(getOrderListByPrincipal(principal));
    }

    /**
     * Adds a product to the order list associated with the authenticated user.
     *
     * @param productId The ID of the product to add to the order list.
     * @param principal The principal representing the authenticated user.
     * @return The DTO representing the updated order list for the main page.
     */
    @Override
    @Transactional
    public OrderListUserInfoDto addProductToOrderList(Long productId, Principal principal) {

        Product productById = utilsService.getProductById(productId);

        OrderList orderList = getOrderListByPrincipal(principal);
        orderList.getProducts().add(productById);
        orderList.setTotalPrice(orderList.getTotalPrice().add(productById.getProductPrice()));
        OrderList savedOrderList = orderListRepository.save(orderList);
        return OrderListMapper.ORDER_LIST_MAPPER_INSTANCE.orderListToOrderUserInfoDto(savedOrderList);
    }

    /**
     * Deletes a product from the order list associated with the authenticated user.
     *
     * @param productId The ID of the product to delete from the order list.
     * @param principal The principal representing the authenticated user.
     * @return The DTO representing the updated order list for the main page.
     * @throws ResponseStatusException if the product does not exist in the order list.
     */
    @Override
    @Transactional
    public OrderListUserInfoDto deleteFromOrderList(Long productId, Principal principal) {

        Product productById = utilsService.getProductById(productId);
        OrderList orderList = getOrderListByPrincipal(principal);

        isExistInOrderList(orderList, productById);

        orderList.getProducts().remove(productById);
        orderList.setTotalPrice(subtractTotalPrice(orderList,productById));
        OrderList savedOrderList = orderListRepository.save(orderList);
        return OrderListMapper.ORDER_LIST_MAPPER_INSTANCE.orderListToOrderUserInfoDto(savedOrderList);
    }

    private OrderList getOrderListByPrincipal(Principal principal) {
        User userByPrincipal = utilsService.getUserByPrincipal(principal);
        OrderList orderList = userByPrincipal.getOrderList();
        if(orderList == null) {
            OrderList createdOrderList = new OrderList();
            createdOrderList.setUser(userByPrincipal);
            createdOrderList.setProducts(new ArrayList<>());
            return createdOrderList;
        }

            return orderList;
    }

    private void isExistInOrderList(OrderList orderList, Product product) {
        if (!orderList.getProducts().contains(product)) {
            throw new ResponseStatusException(HttpStatus.CONFLICT,
                    String.format(ErrorMessageHandler.PRODUCT_NOT_FOUND, product.getId()));
        }
    }

    private BigDecimal subtractTotalPrice(OrderList orderList, Product product) {
        orderList.setTotalPrice(orderList.getTotalPrice().subtract(product.getProductPrice()));
        if(orderList.getTotalPrice().doubleValue() <= 0){
            return BigDecimal.ZERO;
        } else {
            return orderList.getTotalPrice();
        }
    }
}
