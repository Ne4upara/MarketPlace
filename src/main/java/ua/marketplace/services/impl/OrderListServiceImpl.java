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
        OrderList orderList = getOrderListByPrincipal(principal);
        return convertToOrderListDto(orderList);
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
        Product productById = getProductById(productId);
        OrderList orderList = getOrderListByPrincipal(principal);
        addToOrderList(productById,orderList);
        OrderList savedOrderList = orderListRepository.save(orderList);
        return convertToOrderListUserInfoDto(savedOrderList);
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
        Product productById = getProductById(productId);
        OrderList orderList = getOrderListByPrincipal(principal);
        ensureProductExistsInOrderList(orderList, productById);
        removeFromOrderList(productById, orderList);
        OrderList savedOrderList = orderListRepository.save(orderList);
        return convertToOrderListUserInfoDto(savedOrderList);
    }

    private Product getProductById(Long id) {
        return utilsService.getProductById(id);
    }

    private OrderListUserInfoDto convertToOrderListUserInfoDto(OrderList orderList) {
        return OrderListMapper.ORDER_LIST_MAPPER_INSTANCE.orderListToOrderUserInfoDto(orderList);
    }

    private OrderListDto convertToOrderListDto(OrderList orderList) {
        return OrderListMapper.ORDER_LIST_MAPPER_INSTANCE.orderListToOrderListDto(orderList);
    }

    private OrderList getOrderListByPrincipal(Principal principal) {
        User userByPrincipal = utilsService.getUserByPrincipal(principal);
        OrderList orderList = userByPrincipal.getOrderList();
        if(orderList != null) {
            return orderList;
        }
        return createNewOrderList(userByPrincipal);
    }

    private OrderList createNewOrderList(User user) {
        OrderList newOrderList = new OrderList();
        newOrderList.setUser(user);
        newOrderList.setProducts(new ArrayList<>());
        return newOrderList;
    }

    private void addToOrderList(Product product, OrderList orderList) {
        orderList.getProducts().add(product);
        orderList.setTotalPrice(orderList.getTotalPrice().add(product.getProductPrice()));
    }

    private void removeFromOrderList(Product product, OrderList orderList) {
        orderList.getProducts().remove(product);
        orderList.setTotalPrice(subtractTotalPrice(orderList,product));
    }

    private void ensureProductExistsInOrderList(OrderList orderList, Product product) {
        if (!orderList.getProducts().contains(product)) {
            throw new ResponseStatusException(HttpStatus.CONFLICT,
                    String.format(ErrorMessageHandler.PRODUCT_NOT_FOUND, product.getId()));
        }
    }

    private BigDecimal subtractTotalPrice(OrderList orderList, Product product) {
        BigDecimal newTotalPrice = orderList.getTotalPrice().subtract(product.getProductPrice());
        return newTotalPrice.compareTo(BigDecimal.ZERO) <= 0 ? BigDecimal.ZERO : newTotalPrice;
    }
}
