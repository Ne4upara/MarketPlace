package ua.marketplace.controllers;

import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.RequestBody;
import ua.marketplace.dto.CodeDto;
import ua.marketplace.dto.PhoneNumberDto;
import ua.marketplace.requests.PhoneCodeRequest;
import ua.marketplace.requests.PhoneNumberRequest;
import ua.marketplace.requests.RegistrationRequest;

@Tag(name = "Auth controller",
        description = "Endpoints for AUTH operations.")
public interface IPhoneAuthController {

    PhoneNumberDto inputPhoneNumber(@Valid @RequestBody PhoneNumberRequest request);

    CodeDto inputCode(@Valid @RequestBody PhoneCodeRequest request);

    PhoneNumberDto registration(@Valid @RequestBody RegistrationRequest request);
}