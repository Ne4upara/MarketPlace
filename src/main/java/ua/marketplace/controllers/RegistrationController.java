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
import ua.marketplace.dto.CodeDto;
import ua.marketplace.dto.FacebookDto;
import ua.marketplace.dto.GoogleDto;
import ua.marketplace.dto.PhoneNumberDto;
import ua.marketplace.requests.FaceBookRequest;
import ua.marketplace.requests.GoogleRequest;
import ua.marketplace.requests.PhoneCodeRequest;
import ua.marketplace.requests.PhoneNumberRequest;
import ua.marketplace.responses.CustomResponse;
import ua.marketplace.services.PhoneNumberRegistrationService;

import java.util.List;

/**
 * Controller class for handling authentication-related requests.
 */
@RestController
@RequestMapping("/api/v1/auth")
@RequiredArgsConstructor
public class RegistrationController {

    private final PhoneNumberRegistrationService phoneNumberService;

    /**
     * Handling the registration of a new phone number.
     *
     * @param request PhoneNumberRequest containing the phone number to be registered.
     * @param result  BindingResult for validating the request.
     * @return ResponseEntity with CustomResponse containing the registered phone number or validation errors.
     */
    @PostMapping("/phonenumber/registration")
    public ResponseEntity<CustomResponse<PhoneNumberDto>> inputPhoneNumber(
            @Valid @RequestBody PhoneNumberRequest request, BindingResult result) {

        if (result.hasErrors()) {
            List<String> errorList = result.getAllErrors().stream()
                    .map(DefaultMessageSourceResolvable::getDefaultMessage)
                    .toList();
            return ResponseEntity.badRequest().body(CustomResponse.failed(errorList, HttpStatus.BAD_REQUEST.value()));
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
    @PostMapping("/phonenumber/code/registration")
    public ResponseEntity<CustomResponse<CodeDto>> inputCode
    (@Valid @RequestBody PhoneCodeRequest request, BindingResult result) {

        if (result.hasErrors()) {
            List<String> errorList = result.getAllErrors().stream()
                    .map(DefaultMessageSourceResolvable::getDefaultMessage)
                    .toList();
            return ResponseEntity.badRequest().body(CustomResponse.failed(errorList, HttpStatus.BAD_REQUEST.value()));
        }
        return phoneNumberService.inputPhoneCode(request);
    }

    @PostMapping("google/registration")
    public ResponseEntity<CustomResponse<GoogleDto>> googleRegistration
            (@Valid @RequestBody GoogleRequest request, BindingResult result) {

        if (result.hasErrors()) {
            List<String> errorList = result.getAllErrors().stream()
                    .map(DefaultMessageSourceResolvable::getDefaultMessage)
                    .toList();
            return ResponseEntity.badRequest().body(CustomResponse.failed(errorList, HttpStatus.BAD_REQUEST.value()));
        }
        return null;
    }

    @PostMapping("facebook/registration")
    public ResponseEntity<CustomResponse<FacebookDto>> facebookRegistration
            (@Valid @RequestBody FaceBookRequest request, BindingResult result) {

        if (result.hasErrors()) {
            List<String> errorList = result.getAllErrors().stream()
                    .map(DefaultMessageSourceResolvable::getDefaultMessage)
                    .toList();
            return ResponseEntity.badRequest().body(CustomResponse.failed(errorList, HttpStatus.BAD_REQUEST.value()));
        }
        return null;
    }
}