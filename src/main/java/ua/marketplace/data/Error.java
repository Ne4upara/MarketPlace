package ua.marketplace.data;

import lombok.Getter;

/**
 * Enum representing various error messages.
 */
@Getter
public enum Error {

    OK("OK"),
    PHONE_ALREADY_EXIST("Phone already exist"),
    USER_NOT_FOUND("User with this phone not found"),
    INVALID_CODE("The code was entered incorrectly"),
    TIME_IS_UP("Time is up"),
    INVALID_PASSWORD("Invalid password");

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