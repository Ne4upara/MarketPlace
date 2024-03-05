package ua.marketplace.controllers;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import ua.marketplace.dto.CodeDto;
import ua.marketplace.dto.PhoneNumberDto;
import ua.marketplace.entities.User;
import ua.marketplace.requests.*;
import ua.marketplace.security.JwtUtil;
import ua.marketplace.services.PhoneNumberRegistrationService;

/**
 * Controller class for handling authentication-related requests.
 */
@RestController
@RequestMapping("/api/v1/auth")
@RequiredArgsConstructor
@Tag(name = "Authorization controller",
        description = "Endpoints for registration and authorization users")
public class PhoneAuthController {

    private final PhoneNumberRegistrationService phoneNumberService;
    private final JwtUtil jwtUtil;

    /**
     * Handling the registration of a new phone number.
     *
     * @param request PhoneNumberRequest containing the phone number to be registered.
     * @return ResponseEntity containing the registered phone number or validation errors.
     */
    @PostMapping("/login")
    @ResponseStatus(HttpStatus.OK)
    @Operation(summary = "User login",
            description = "Endpoint for sending user verification codes")
    public PhoneNumberDto inputPhoneNumber(
            @Valid @RequestBody PhoneNumberRequest request){
        User user = phoneNumberService.inputPhoneNumber(request);
        return new PhoneNumberDto(user.getPhoneNumber());
    }

    /**
     * Handling the input of a phone code for registration.
     *
     * @param request PhoneCodeRequest containing the phone number and input code.
     * @return ResponseEntity containing the JWT token or validation errors.
     */
    @PostMapping("/login/code")
    @ResponseStatus(HttpStatus.OK)
    @Operation(summary = "Verification SMS-code",
            description = "Endpoint for verification SMS-code")
    public CodeDto inputCode
    (@Valid @RequestBody PhoneCodeRequest request){
        User user = phoneNumberService.inputPhoneCode(request);
        return new CodeDto(jwtUtil.generateToken(user.getPhoneNumber()), user.getFirstName());
    }

    /**
     * Endpoint for handling user registration.
     *
     * @param request The registration request containing user details.
     * @return ResponseEntity containing the response for the registration request.
     */
    @PostMapping("/registration")
    @ResponseStatus(HttpStatus.ACCEPTED)
    @Operation(summary = "Registration new user" ,
            description = "Endpoint for registration new users")
    public PhoneNumberDto registration
    (@Valid @RequestBody RegistrationRequest request){
        User user = phoneNumberService.registrationUser(request);
        return new PhoneNumberDto(user.getPhoneNumber());
    }
}