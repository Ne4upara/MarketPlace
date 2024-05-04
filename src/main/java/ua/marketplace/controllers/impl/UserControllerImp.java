package ua.marketplace.controllers.impl;

import io.micrometer.core.annotation.Timed;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import ua.marketplace.controllers.IUserController;
import ua.marketplace.dto.Pagination;
import ua.marketplace.dto.UserDto;
import ua.marketplace.services.impl.UserService;

import java.security.Principal;

/**
 * Controller class for managing user-related endpoints.
 * Handles HTTP requests related to user profiles and actions.
 */
@RestController
@RequestMapping("/v1/my-profile")
@RequiredArgsConstructor
public class UserControllerImp implements IUserController {

    private final UserService userService;

    /**
     * Retrieves all products associated with the logged-in user.
     *
     * @param number The page number for pagination (default: 0).
     * @param size The page size for pagination (default: 10).
     * @param sort The field to sort by (default: "creationDate").
     * @param order The sort order (default: "ASC").
     * @param principal The principal object representing the logged-in user.
     * @return A Pagination object containing information about the user's products.
     */
    @GetMapping("/view/all")
    @ResponseStatus(HttpStatus.OK)
    @Timed("getViewUserProduct")
    public Pagination getViewMyProduct(
            @Valid @RequestParam(defaultValue = "0") @PositiveOrZero int number,
            @Valid @RequestParam(defaultValue = "10") @Positive int size,
            @Valid @RequestParam(defaultValue = "creationDate")
            @Pattern(regexp = "creationDate|productName|productPrice|id") String sort,
            @Valid @RequestParam(defaultValue = "ASC") @Pattern(regexp = "ASC|DESC") String order,
            Principal principal){
        return userService.getViewMyProduct(number, size, sort, order, principal);
    }

    /**
     * Retrieves information about the logged-in user.
     *
     * @param principal The principal object representing the logged-in user.
     * @return A UserDto object containing information about the user.
     */
    @GetMapping("/info")
    @ResponseStatus(HttpStatus.OK)
    public UserDto getUserInfo(Principal principal){
        return userService.getUserInfo(principal);
    }

    /**
     * Retrieves all favorite products associated with the logged-in user.
     *
     * @param number The page number for pagination (default: 0).
     * @param size The page size for pagination (default: 10).
     * @param sort The field to sort by (default: "creationDate").
     * @param order The sort order (default: "ASC").
     * @param principal The principal object representing the logged-in user.
     * @return A Pagination object containing information about the user's favorite products.
     */
    @GetMapping("/favorite/all")
    @ResponseStatus(HttpStatus.OK)
    @Timed("getAllFavoriteForUser")
    public Pagination getAllFavorite(
        @Valid @RequestParam(defaultValue = "0") @PositiveOrZero int number,
        @Valid @RequestParam(defaultValue = "10") @Positive int size,
        @Valid @RequestParam(defaultValue = "creationDate")
        @Pattern(regexp = "creationDate|productName|productPrice|id") String sort,
        @Valid @RequestParam(defaultValue = "ASC") @Pattern(regexp = "ASC|DESC") String order,
        Principal principal){
        return userService.getAllFavorite(number, size, sort, order, principal);
    }
}
