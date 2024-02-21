package ua.marketplace.controllers;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
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
import ua.marketplace.dto.CodeDto;
import ua.marketplace.dto.PhoneNumberDto;
import ua.marketplace.requests.*;
import ua.marketplace.responses.CustomResponse;
import ua.marketplace.services.PhoneNumberRegistrationService;

import java.util.List;

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

    /**
     * Handling the registration of a new phone number.
     *
     * @param request PhoneNumberRequest containing the phone number to be registered.
     * @param result  BindingResult for validating the request.
     * @return ResponseEntity with CustomResponse containing the registered phone number or validation errors.
     */
    @PostMapping("/login")
    @Operation(summary = "User login",
            description = "Endpoint for sending user verification codes")
    public ResponseEntity<CustomResponse<PhoneNumberDto>> inputPhoneNumber(
            @Valid @RequestBody PhoneNumberRequest request, BindingResult result) {
        if (result.hasErrors()) {
            return ResponseEntity.badRequest()
                    .body(CustomResponse.failed(getAllErrorList(result), HttpStatus.BAD_REQUEST.value()));
        }

        return phoneNumberService.inputPhoneNumber(request);
    }

    /**
     * Handling the input of a phone code for registration.
     *
     * @param request PhoneCodeRequest containing the phone number and input code.
     * @param result  BindingResult for validating the request.
     * @return ResponseEntity with CustomResponse containing the JWT token or validation errors.
     */
    @PostMapping("/login/code")
    @Operation(summary = "Verification SMS-code",
            description = "Endpoint for verification SMS-code")
    public ResponseEntity<CustomResponse<CodeDto>> inputCode
    (@Valid @RequestBody PhoneCodeRequest request, BindingResult result) {
        if (result.hasErrors()) {
            return ResponseEntity.badRequest()
                    .body(CustomResponse.failed(getAllErrorList(result), HttpStatus.BAD_REQUEST.value()));
        }

        return phoneNumberService.inputPhoneCode(request);
    }

    /**
     * Endpoint for handling user registration.
     *
     * @param request The registration request containing user details.
     * @param result The binding result for validation errors.
     * @return ResponseEntity containing the response for the registration request.
     */
    @PostMapping("/registration")
    @Operation(summary = "Registration new user" ,
            description = "Endpoint for registration new users")
    public ResponseEntity<CustomResponse<PhoneNumberDto>> registration
    (@Valid @RequestBody RegistrationRequest request, BindingResult result) {
        if (result.hasErrors()) {
            return ResponseEntity.badRequest()
                    .body(CustomResponse.failed(getAllErrorList(result), HttpStatus.BAD_REQUEST.value()));
        }

        return phoneNumberService.registrationUser(request);
    }

    private List<String> getAllErrorList(BindingResult result) {
        return result.getAllErrors().stream()
                .map(DefaultMessageSourceResolvable::getDefaultMessage)
                .toList();
    }
}
