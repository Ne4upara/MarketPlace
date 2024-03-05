package ua.marketplace.utils;

public final class ErrorMessageHandler {
    public static final String USER_NOT_AUTHORIZED = "User not authorized";
    public static final String FAILED_PRODUCT_UPDATE = "Failed to update product";
    public static final String PRODUCT_NOT_FOUND = "Not found product with ID: ";
    public static final String PRODUCT_RATING_ERROR = "Rating must be between 0 and 5";
    public static final String DELETING_WITH_NOT_AUTHORIZED_USER = "You are not authorized to delete this product";
    public static final String ERROR_MESSAGE = "errorMessage";
    public static final String NOT_AUTHORIZED = "You are not authorized to update this product";
    public static final String PHONE_ALREADY_EXIST = "Phone already exists: %d";
    public static final String TIME_IS_UP = "Time is up";
    public static final String USED_UP_ALL = "You've used up all your attempts";
    public static final String CODE_WAS_ENTERED_INCORRECT = "The code was entered incorrectly";
    public static final String CODE_ALREADY_ENTRY = "There was already a code entry.";
    public static final String SEND_REPEAT = "Time to send a repeat code 1 minute";
    public static final String USER_NOT_FOUND = "User with this phone not found %d";

    private ErrorMessageHandler(){}
}
