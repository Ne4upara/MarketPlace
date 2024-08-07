package ua.marketplace.controllers.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import ua.marketplace.controllers.OrderListController;
import ua.marketplace.dto.OrderListDto;
import ua.marketplace.dto.OrderListUserInfoDto;
import ua.marketplace.services.impl.OrderListServiceImpl;

import java.security.Principal;

/**
 * Controller class for managing order list.
 * Handles requests related to order list.
 */
@RestController
@RequestMapping("/v1/order_list")
@RequiredArgsConstructor
public class OrderListControllerImp implements OrderListController {

    private final OrderListServiceImpl orderListService;

    /**
     * Retrieves the order list for the authenticated user.
     *
     * @param principal The authenticated user's principal object.
     * @return The DTO representing the order list.
     */
    @GetMapping("/view")
    public OrderListDto getOrderList(Principal principal) {
       return orderListService.viewOrderList(principal);
    }

    /**
     * Adds a product to the order list for the authenticated user.
     *
     * @param principal The authenticated user's principal object.
     * @param id The ID of the product to add to the order list.
     * @return The DTO representing the updated order list for the main page.
     */
    @PostMapping("/add/{id}")
    @ResponseStatus(HttpStatus.ACCEPTED)
    public OrderListUserInfoDto addProductToOrderList(Principal principal, @PathVariable Long id) {
        return orderListService.addProductToOrderList(id,principal);
    }

    /**
     * Deletes a product from the order list for the authenticated user.
     *
     * @param principal The authenticated user's principal object.
     * @param id The ID of the product to delete from the order list.
     * @return The DTO representing the updated order list for the main page.
     */
    @DeleteMapping("/delete/{id}")
    @ResponseStatus(HttpStatus.ACCEPTED)
    public OrderListUserInfoDto deleteProductFromOrderList(Principal principal, @PathVariable Long id) {
        return orderListService.deleteFromOrderList(id,principal);
    }
}
