package ua.marketplace.controllers;

import io.micrometer.core.annotation.Counted;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import ua.marketplace.dto.CodeDto;
import ua.marketplace.dto.PhoneNumberDto;
import ua.marketplace.entities.User;
import ua.marketplace.requests.PhoneCodeRequest;
import ua.marketplace.requests.PhoneNumberRequest;
import ua.marketplace.requests.RegistrationRequest;
import ua.marketplace.security.JwtUtil;
import ua.marketplace.services.PhoneNumberRegistrationService;

/**
 * Controller class for handling authentication-related requests.
 */
@RestController
@RequestMapping("/v1/auth")
@RequiredArgsConstructor
public class PhoneAuthControllerImp implements IPhoneAuthController {

    private final PhoneNumberRegistrationService phoneNumberService;
    private final JwtUtil jwtUtil;

    /**
     * Handling the registration of a new phone number.
     *
     * @param request PhoneNumberRequest containing the phone number to be registered.
     * @return PhoneNumberDto containing the registered phone number or validation errors.
     */
    @PostMapping("/login")
    @ResponseStatus(HttpStatus.OK)
    public PhoneNumberDto inputPhoneNumber(
            @Valid @RequestBody PhoneNumberRequest request) {
        User user = phoneNumberService.inputPhoneNumber(request);
        return new PhoneNumberDto(user.getPhoneNumber());
    }

    /**
     * Handling the input of a phone code for registration.
     *
     * @param request PhoneCodeRequest containing the phone number and input code.
     * @return CodeDto containing the JWT token or validation errors.
     */
    @PostMapping("/login/code")
    @ResponseStatus(HttpStatus.OK)
    public CodeDto inputCode
    (@Valid @RequestBody PhoneCodeRequest request) {
        User user = phoneNumberService.inputPhoneCode(request);
        return new CodeDto(jwtUtil.generateToken(user.getPhoneNumber()));
    }

    /**
     * Endpoint for handling user registration.
     *
     * @param request The registration request containing user details.
     * @return PhoneNumberDto containing the response for the registration request.
     */
    @PostMapping("/registration")
    @Counted(value = "login.code.requests", description = "Number of requests to login code endpoint")
    @ResponseStatus(HttpStatus.ACCEPTED)
    public PhoneNumberDto registration
    (@Valid @RequestBody RegistrationRequest request) {
        User user = phoneNumberService.registrationUser(request);
        return new PhoneNumberDto(user.getPhoneNumber());
    }

    /**
     * Handles user logout by invalidating the JWT token.
     *
     * @param request The HTTP request.
     * @param session The HTTP session.
     */
    @PostMapping("/logout")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void logout(HttpServletRequest request, HttpSession session) {
        String authorizationHeader = request.getHeader("Authorization");
        if (authorizationHeader != null && authorizationHeader.startsWith("Bearer ")) {
            String jwt = authorizationHeader.substring(7);
            jwtUtil.killToken(jwt);
        }

        session.invalidate();
    }

}

