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
import ua.marketplace.requests.CheckCodeRequest;
import ua.marketplace.requests.LoginRequest;
import ua.marketplace.requests.RegistrationRequest;
import ua.marketplace.responses.CustomResponse;
import ua.marketplace.services.PhoneAuthService;

import java.util.List;

@RestController
@RequestMapping("/api/v1/auth")
@RequiredArgsConstructor
public class RegistrationController {

    private final PhoneAuthService service;

    @PostMapping("/reg")
    public ResponseEntity<CustomResponse<UserDto>> registration
            (@RequestBody @Valid RegistrationRequest request, BindingResult result) {

        if (result.hasErrors()) {
            List<String> errorList = result.getAllErrors().stream()
                    .map(DefaultMessageSourceResolvable::getDefaultMessage)
                    .toList();
            return ResponseEntity.badRequest().body(CustomResponse.failed(errorList, HttpStatus.BAD_REQUEST.value()));
        }
        return service.registration(request);
    }

    @PostMapping("/log")
    public ResponseEntity<CustomResponse<UserDto>> login
            (@RequestBody @Valid LoginRequest request, BindingResult result) {

        if (result.hasErrors()) {
            List<String> errorList = result.getAllErrors().stream()
                    .map(DefaultMessageSourceResolvable::getDefaultMessage)
                    .toList();
            return ResponseEntity.badRequest().body(CustomResponse.failed(errorList, HttpStatus.BAD_REQUEST.value()));
        }

        return service.login(request);
    }

    @PostMapping("/log/code")
    public ResponseEntity<CustomResponse<AuthDto>> checkCode
            (@RequestBody @Valid CheckCodeRequest request, BindingResult result) {

        if (result.hasErrors()) {
            List<String> errorList = result.getAllErrors().stream()
                    .map(DefaultMessageSourceResolvable::getDefaultMessage)
                    .toList();
            return ResponseEntity.badRequest().body(CustomResponse.failed(errorList, HttpStatus.BAD_REQUEST.value()));
        }

        return service.checkCode(request);
    }
}