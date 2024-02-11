package ua.marketplace.services;

import org.springframework.http.ResponseEntity;
import ua.marketplace.dto.CodeDto;
import ua.marketplace.dto.FacebookDto;
import ua.marketplace.dto.GoogleDto;
import ua.marketplace.dto.PhoneNumberDto;
import ua.marketplace.requests.FaceBookRequest;
import ua.marketplace.requests.GoogleRequest;
import ua.marketplace.requests.PhoneCodeRequest;
import ua.marketplace.requests.PhoneNumberRequest;
import ua.marketplace.responses.CustomResponse;

/**
 * Interface defining the contract for phone number registration service.
 */
public interface IPhoneNumberRegistrationService {

    /**
     * Handles the registration of a new phone number.
     *
     * @param request PhoneNumberRequest containing the phone number to be registered.
     * @return ResponseEntity with CustomResponse containing the registered phone number or error message.
     */
    ResponseEntity<CustomResponse<PhoneNumberDto>> inputPhoneNumber(PhoneNumberRequest request);

    /**
     * Handles the input of a phone code during registration.
     *
     * @param request PhoneCodeRequest containing the phone number and input code.
     * @return ResponseEntity with CustomResponse containing the JWT token or error message.
     */
    ResponseEntity<CustomResponse<CodeDto>> inputPhoneCode(PhoneCodeRequest request);
}