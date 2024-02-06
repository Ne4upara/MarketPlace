package ua.marketplace.auth;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import ua.marketplace.auth.login.LoginRequest;
import ua.marketplace.auth.login.LoginResponse;
import ua.marketplace.auth.registration.RegistrationRequest;
import ua.marketplace.auth.registration.RegistrationResponse;
import ua.marketplace.config.ValidationConfig;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/v1/auth")
@Tag(name = "Authentication Controller",
        description = "Endpoints for user registration and login")
public class AuthController {

    private final AuthService authService;
    private final ValidationConfig validationConfig;

    @Operation(summary = "Register a new user",
            description = "Endpoint to register a new user.")
    @PostMapping("/register")
    public RegistrationResponse register(@Valid @RequestBody RegistrationRequest request) {
        return authService.register(request);
    }

    @Operation(summary = "User login",
            description = "Endpoint for user login.")
    @PostMapping("/login")
    public LoginResponse login(@RequestBody LoginRequest request) {
        return authService.login(request);
    }
}