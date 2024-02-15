package ua.marketplace.data;

import lombok.Getter;

/**
 * Enum representing various error messages.
 */
@Getter
public enum Error {

    PHONE_ALREADY_EXIST("Phone already exist"),
    USER_NOT_FOUND("User with this phone number not found"),
    INVALID_CODE("Invalid code"),
    CODE_TIME_IS_OUT("Code time is out");

    private final String message;

    /**
     * Constructor to initialize an Error with a message.
     *
     * @param message the error message
     */
    Error(String message) {
        this.message = message;
    }
}
