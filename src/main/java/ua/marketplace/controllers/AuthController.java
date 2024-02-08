package ua.marketplace.controllers;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.context.support.DefaultMessageSourceResolvable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import ua.marketplace.dto.AuthDto;
import ua.marketplace.dto.UserDto;
import ua.marketplace.requests.LoginRequest;
import ua.marketplace.requests.RegisterRequest;
import ua.marketplace.responses.CustomResponse;
import ua.marketplace.services.AuthService;

import java.util.List;

/**
 * Controller class for handling authentication-related requests.
 */
@RestController
@RequestMapping("/api/v1/auth")
@RequiredArgsConstructor
public class AuthController {

    private final AuthService service;

    /**
     * Endpoint for user registration.
     *
     * @param request the registration request containing user data
     * @param result  the binding result for validation errors
     * @return response entity with registration result
     */
    @PostMapping("/reg")
    public ResponseEntity<CustomResponse<UserDto>> register
    (@Valid @RequestBody RegisterRequest request, BindingResult result) {

        if (result.hasErrors()) {
            List<String> errorList = result.getAllErrors().stream()
                    .map(DefaultMessageSourceResolvable::getDefaultMessage)
                    .toList();
            return ResponseEntity.badRequest().body(CustomResponse.failed(errorList, HttpStatus.BAD_REQUEST.value()));
        }

        return service.registration(request);
    }

    /**
     * Endpoint for user login.
     *
     * @param request the login request containing user credentials
     * @return response entity with login result
     */
    @PostMapping("/login")
    public ResponseEntity<CustomResponse<AuthDto>> login(@RequestBody LoginRequest request) {
        return service.login(request);
    }
}