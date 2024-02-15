package ua.marketplace.controllers;

import jakarta.servlet.http.HttpServletRequest;
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
import ua.marketplace.requests.CheckCodeRequest;
import ua.marketplace.requests.LoginRequest;
import ua.marketplace.requests.RegistrationRequest;
import ua.marketplace.responses.CustomResponse;
import ua.marketplace.services.PhoneAuthService;

import java.util.List;

/**
 * Controller class for handling phone number authentication-related endpoints.
 */
@RestController
@RequestMapping("/api/v1/auth/phone/")
@RequiredArgsConstructor
public class PhoneAuthController {

    private final PhoneAuthService service;

    /**
     * Endpoint for user registerUser via phone number.
     *
     * @param request RegistrationRequest object containing user's registerUser data.
     * @param result  BindingResult for validating the request body.
     * @return ResponseEntity containing CustomResponse with UserDto if registerUser is successful,
     * or a bad request response with error messages if validation fails.
     */
    @PostMapping("/registration")
    public ResponseEntity<CustomResponse<UserDto>> registration
    (@RequestBody @Valid RegistrationRequest request, BindingResult result) {

        if (result.hasErrors()) {
            List<String> errorList = result.getAllErrors().stream()
                    .map(DefaultMessageSourceResolvable::getDefaultMessage)
                    .toList();

            return ResponseEntity.badRequest().body(CustomResponse.failed(errorList, HttpStatus.BAD_REQUEST.value()));
        }

        return service.registerUser(request);
    }

    /**
     * Endpoint for user loginUser via phone number.
     *
     * @param request LoginRequest object containing user's loginUser data.
     * @param result  BindingResult for validating the request body.
     * @param httpServletRequest HttpServletRequest to access the session.
     * @return ResponseEntity containing CustomResponse with UserDto if loginUser is successful,
     * or a bad request response with error messages if validation fails.
     */
    @PostMapping("/login")
    public ResponseEntity<CustomResponse<UserDto>> login
    (@RequestBody @Valid LoginRequest request, BindingResult result, HttpServletRequest httpServletRequest) {

        if (result.hasErrors()) {
            return ResponseEntity.badRequest().body(CustomResponse.failed(getErrorMessageList(result),
                    HttpStatus.BAD_REQUEST.value()));
        }

        httpServletRequest.getSession().setAttribute("phoneNumber", request.getPhoneNumber());

        return service.loginUser(request);
    }

    /**
     * Endpoint for verifying loginUser code sent to the user's phone number.
     *
     * @param request CheckCodeRequest object containing the code to be verified.
     * @param result  BindingResult for validating the request body.
     * @param httpServletRequest HttpServletRequest to access the session.
     * @return ResponseEntity containing CustomResponse with AuthDto if code verification is successful,
     * or a bad request response with error messages if validation fails.
     */
    @PostMapping("/login/code")
    public ResponseEntity<CustomResponse<AuthDto>> checkCode
    (@RequestBody @Valid CheckCodeRequest request, BindingResult result, HttpServletRequest httpServletRequest) {

        if (result.hasErrors()) {
            return ResponseEntity.badRequest().body(CustomResponse.failed(getErrorMessageList(result),
                    HttpStatus.BAD_REQUEST.value()));
        }

        String phoneNumber = (String) httpServletRequest.getSession().getAttribute("phoneNumber");
        request.setPhoneNumber(phoneNumber);

        return service.checkVerificationCode(request);
    }

    /**
     * Helper method to extract error messages from BindingResult.
     *
     * @param bindingResult BindingResult containing validation errors.
     * @return List of error messages.
     */
    private List<String> getErrorMessageList(BindingResult bindingResult) {
        return bindingResult.getAllErrors().stream()
                .map(DefaultMessageSourceResolvable::getDefaultMessage)
                .toList();
    }
}
