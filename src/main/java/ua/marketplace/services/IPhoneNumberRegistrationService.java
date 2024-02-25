package ua.marketplace.services;

import ua.marketplace.entities.User;
import ua.marketplace.exception.AppException;
import ua.marketplace.requests.PhoneCodeRequest;
import ua.marketplace.requests.PhoneNumberRequest;
import ua.marketplace.requests.RegistrationRequest;


/**
 * Interface defining the contract for phone number registration service and login transactions.
 */
public interface IPhoneNumberRegistrationService {

    /**
     * Handles login by phone number.
     *
     * @param request PhoneNumberRequest containing the phone number to be registered.
     * @return ResponseEntity with CustomResponse containing the registered phone number or error message.
     */
    User inputPhoneNumber(PhoneNumberRequest request) throws AppException;

    /**
     * Handles the input of a phone code during registration.
     *
     * @param request PhoneCodeRequest containing the phone number and input code.
     * @return ResponseEntity with CustomResponse containing the JWT token and username or error message.
     */
    User inputPhoneCode(PhoneCodeRequest request) throws AppException;

    /**
     * Handles the registration of a new phone number.
     *
     * @param request RegistrationRequest containing the username and phone number
     * @return ResponseEntity with CustomResponse containing the registered phone number or error message.
     */
    User registrationUser(RegistrationRequest request) throws AppException;
}