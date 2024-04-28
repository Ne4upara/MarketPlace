package ua.marketplace.controllers;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import ua.marketplace.dto.Pagination;
import ua.marketplace.services.UserService;

import java.security.Principal;

@RestController
@RequestMapping("/v1/my-profile")
@RequiredArgsConstructor
public class UserControllerImp implements IUserController{

    private final UserService userService;

    @GetMapping("/view/all")
    @ResponseStatus(HttpStatus.OK)
    public Pagination getViewMyProduct(
            @Valid @RequestParam(defaultValue = "0") @PositiveOrZero int number,
            @Valid @RequestParam(defaultValue = "10") @Positive int size,
            @Valid @RequestParam(defaultValue = "creationDate")
            @Pattern(regexp = "creationDate|productName|productPrice|id") String sort,
            @Valid @RequestParam(defaultValue = "ASC") @Pattern(regexp = "ASC|DESC") String order,
            Principal principal){
        return userService.getViewMyProduct(number, size, sort, order, principal);
    }
}
