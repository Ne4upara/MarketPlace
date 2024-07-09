package ua.marketplace.services;

import ua.marketplace.dto.OrderListDto;
import ua.marketplace.dto.OrderListUserInfoDto;

import java.security.Principal;

/**
 * Interface for managing order lists.
 * Provides methods for viewing, adding to, and deleting from an order list.
 */
public interface OrderListService {
    /**
     * Retrieves the order list associated with the authenticated user.
     *
     * @param principal The principal representing the authenticated user.
     * @return The DTO representing the order list.
     */
    OrderListDto viewOrderList(Principal principal);

    /**
     * Adds a product to the order list associated with the authenticated user.
     *
     * @param productId The ID of the product to add to the order list.
     * @param principal The principal representing the authenticated user.
     * @return The DTO representing the updated order list for the main page.
     */
    OrderListUserInfoDto addProductToOrderList(Long productId, Principal principal);

    /**
     * Deletes a product from the order list associated with the authenticated user.
     *
     * @param productId The ID of the product to delete from the order list.
     * @param principal The principal representing the authenticated user.
     * @return The DTO representing the updated order list for the main page.
     */
    OrderListUserInfoDto deleteFromOrderList(Long productId, Principal principal);
}
