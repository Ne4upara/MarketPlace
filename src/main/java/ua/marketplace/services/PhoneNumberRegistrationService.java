package ua.marketplace.services;

import ua.marketplace.entities.User;
import ua.marketplace.requests.PhoneCodeRequest;
import ua.marketplace.requests.PhoneNumberRequest;
import ua.marketplace.requests.RegistrationRequest;


/**
 * Interface defining the contract for phone number registration service and login transactions.
 */
public interface PhoneNumberRegistrationService {

    /**
     * Handles login by phone number.
     *
     * @param request PhoneNumberRequest containing the phone number to be registered.
     * @return User representing the registered user.
     */
    User inputPhoneNumber(PhoneNumberRequest request);

    /**
     * Handles the input of a phone code during registration.
     *
     * @param request PhoneCodeRequest containing the phone number and input code.
     * @return User representing the registered user.
     */
    User inputPhoneCode(PhoneCodeRequest request);

    /**
     * Handles the registration of a new phone number.
     *
     * @param request RegistrationRequest containing the username and phone number.
     * @return User representing the registered user.
     */
    User registration(RegistrationRequest request);
}
