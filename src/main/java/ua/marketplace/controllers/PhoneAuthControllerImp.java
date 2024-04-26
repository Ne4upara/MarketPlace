package ua.marketplace.controllers;

import io.micrometer.core.annotation.Counted; // Imported for counting the number of requests to the login code endpoint
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import jakarta.validation.Valid; // Imported for validating the request objects
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*; // Imported for handling HTTP requests
import ua.marketplace.dto.CodeDto; // Imported for returning the JWT token and user's first name
import ua.marketplace.dto.PhoneNumberDto; // Imported for returning the registered phone number
import ua.marketplace.entities.User;
import ua.marketplace.requests.PhoneCodeRequest; // Imported for handling the input of a phone code for registration
import ua.marketplace.requests.PhoneNumberRequest; // Imported for handling the registration of a new phone number
import ua.marketplace.requests.RegistrationRequest; // Imported for handling user registration
import ua.marketplace.security.JwtUtil; // Imported for generating and invalidating JWT tokens
import ua.marketplace.services.PhoneNumberRegistrationService; // Imported for handling phone number-related operations

/**
 * Controller class for handling authentication-related requests.
 */
@RestController // Indicates that this class is a RESTful controller
@RequestMapping("/v1/auth") // Defines the base URL path for all endpoints in this class
@RequiredArgsConstructor // Generates a constructor with required arguments
public class PhoneAuthControllerImp implements IPhoneAuthController {

    private final PhoneNumberRegistrationService phoneNumberService; // Autowired dependency for handling phone number-related operations
    private final JwtUtil jwtUtil; // Autowired dependency for generating and invalidating JWT tokens

    /**
     * Handling the registration of a new phone number.
     *
     * @param request PhoneNumberRequest containing the phone number to be registered.
     * @return PhoneNumberDto containing the registered phone number or validation errors.
     */
    @PostMapping("/login") // Maps this method to the POST /v1/auth/login endpoint
    @ResponseStatus(HttpStatus.OK) // Sets the HTTP status code to 200 OK
    public PhoneNumberDto inputPhoneNumber(
            @Valid @RequestBody PhoneNumberRequest request) { // Validates and extracts the request object from the request body
        User user = phoneNumberService.inputPhoneNumber(request); // Calls the service method to register the phone number
        return new PhoneNumberDto(user.getPhoneNumber()); // Returns a PhoneNumberDto containing the registered phone number
    }

    /**
     * Handling the input of a phone code for registration.
     *
     * @param request PhoneCodeRequest containing the phone number and input code.
     * @return CodeDto containing the JWT token or validation errors.
     */
    @PostMapping("/login/code") // Maps this method to the POST /v1/auth/login/code endpoint
    @ResponseStatus(HttpStatus.OK) // Sets the HTTP status code to 200 OK
    public CodeDto inputCode
    (@Valid @RequestBody PhoneCodeRequest request) { // Validates and extracts the request object from the request body
        User user = phoneNumberService.inputPhoneCode(request); // Calls the service method to register the phone number with the provided code
        return new CodeDto(jwtUtil.generateToken(user.getPhoneNumber()), user.getFirstName()); // Returns a CodeDto containing the JWT token and user's first name
    }

    /**
     * Endpoint for handling user registration.
     *
     * @param request The registration request containing user details.
     * @return PhoneNumberDto containing the response for the registration request.
     */
    @PostMapping("/registration") // Maps this method to the POST /v1/auth/registration endpoint
    @Counted(value = "login.code.requests", description = "Number of requests to login code endpoint") // Counts the number of requests to this endpoint
    @ResponseStatus(HttpStatus.ACCEPTED) // Sets the HTTP status code to 202 Accepted
    public PhoneNumberDto registration
    (@Valid @RequestBody RegistrationRequest request) { // Validates and extracts the request object from the request body
        User user = phoneNumberService.registrationUser(request); // Calls the service method to register the user
        return new PhoneNumberDto(user.getPhoneNumber()); // Returns a PhoneNumberDto containing the registered phone number
    }

    /**
     * Handles user logout by invalidating the JWT token.
     *
     * @param request The HTTP request.
     * @param session The HTTP session.
     */
    @PostMapping("/logout") // Maps this method to the POST /v1/auth/logout endpoint
    @ResponseStatus(HttpStatus.NO_CONTENT) // Sets the HTTP status code to 204 No Content
    public void logout(HttpServletRequest request, HttpSession session) {
        String authorizationHeader = request.getHeader("Authorization"); // Extracts the Authorization header from the request
        if (authorizationHeader != null && authorizationHeader.startsWith("Bearer ")) { // Checks if the Authorization header is present and starts with "Bearer "
            String jwt = authorizationHeader.substring(7); // Extracts the JWT token from the Authorization header
            jwtUtil.killToken(jwt); // Invalidates the JWT token
        }

        session.invalidate(); // Invalidate the HTTP session
    }

}

