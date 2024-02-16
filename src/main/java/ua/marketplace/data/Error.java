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
    CODE_TIME_IS_OUT("Code time is out, please generate a new code"),
    CODE_NOT_ENABLE("This code has already been used, please generate a new code"),
    TRY_IS_OUT("You have used all attempts to enter the code, please generate a new code");

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
