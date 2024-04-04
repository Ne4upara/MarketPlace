package ua.marketplace.utils;

/**
 * Class containing static final strings representing error messages used in the application.
 */
public final class ErrorMessageHandler {
    public static final String USER_NOT_AUTHORIZED = "User not authorized";
    public static final String FAILED_PRODUCT_UPDATE = "Failed to update product";
    public static final String THIS_NOT_USERS_PRODUCT = "This product was not created by this user";
    public static final String PRODUCT_NOT_FOUND = "Not found product with ID: ";
    public static final String PRODUCT_RATING_ERROR = "Rating must be between 0 and 5";
    public static final String ERROR_MESSAGE = "errorMessage";
    public static final String PHONE_ALREADY_EXIST = "Phone already exists: %s";
    public static final String TIME_IS_UP = "Time is up";
    public static final String USED_UP_ALL = "You've used up all your attempts";
    public static final String CODE_WAS_ENTERED_INCORRECT = "The code was entered incorrectly";
    public static final String CODE_ALREADY_ENTRY = "There was already a code entry.";
    public static final String SEND_REPEAT = "Time to send a repeat code 1 minute";
    public static final String USER_NOT_FOUND = "User with this phone not found %s";
    public static final String INVALID_CATEGORY = "Invalid category: %s";

    private ErrorMessageHandler() {
    }
}
