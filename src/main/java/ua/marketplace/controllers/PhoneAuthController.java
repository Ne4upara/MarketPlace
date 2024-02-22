package ua.marketplace.controllers;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import ua.marketplace.dto.CodeDto;
import ua.marketplace.dto.PhoneNumberDto;
import ua.marketplace.entities.User;
import ua.marketplace.exception.AppException;
import ua.marketplace.requests.*;
import ua.marketplace.security.JwtUtil;
import ua.marketplace.services.PhoneNumberRegistrationService;

/**
 * Controller class for handling authentication-related requests.
 */
@RestController
@RequestMapping("/api/v1/auth")
@RequiredArgsConstructor
public class PhoneAuthController {

    private final PhoneNumberRegistrationService phoneNumberService;
    private final JwtUtil jwtUtil;

    /**
     * Handling the registration of a new phone number.
     *
     * @param request PhoneNumberRequest containing the phone number to be registered.
     * @return ResponseEntity with CustomResponse containing the registered phone number or validation errors.
     */
    @PostMapping("/login")
    public ResponseEntity<PhoneNumberDto> inputPhoneNumber(
            @Valid @RequestBody PhoneNumberRequest request) throws AppException {
        User user = phoneNumberService.inputPhoneNumber(request);
        return ResponseEntity.status(HttpStatus.OK).body(new PhoneNumberDto(user.getPhoneNumber()));
    }

    /**
     * Handling the input of a phone code for registration.
     *
     * @param request PhoneCodeRequest containing the phone number and input code.
     * @return ResponseEntity with CustomResponse containing the JWT token or validation errors.
     */
    @PostMapping("/login/code")
    public ResponseEntity<CodeDto> inputCode
    (@Valid @RequestBody PhoneCodeRequest request) throws AppException {
        User user = phoneNumberService.inputPhoneCode(request);
        return ResponseEntity.status(HttpStatus.OK)
                .body(new CodeDto(jwtUtil.generateToken(user.getPhoneNumber()), user.getFirstName()));
    }

    /**
     * Endpoint for handling user registration.
     *
     * @param request The registration request containing user details.
     * @return ResponseEntity containing the response for the registration request.
     */
    @PostMapping("/registration")
    public ResponseEntity<PhoneNumberDto> registration
    (@Valid @RequestBody RegistrationRequest request) throws AppException {
        User user = phoneNumberService.registrationUser(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(new PhoneNumberDto(user.getPhoneNumber()));
    }
}