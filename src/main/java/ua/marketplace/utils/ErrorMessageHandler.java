package ua.marketplace.utils;

/**
 * This class, {@code ErrorMessageHandler}, contains static final strings representing
 * error messages used throughout the application. By using these predefined error
 * messages, the application can maintain consistency and improve readability.
 * The error messages cover various scenarios, such as user authorization, product
 * updates, invalid input, and more.
 */
public final class ErrorMessageHandler {

    public static final String USER_NOT_AUTHORIZED = "User not authorized";
    public static final String FAILED_PRODUCT_UPDATE = "Failed to update product";
    public static final String THIS_NOT_USERS_PRODUCT = "This product was not created by this user";
    public static final String PRODUCT_NOT_FOUND = "Not found product with ID: %s";
    public static final String ERROR_MESSAGE = "errorMessage";
    public static final String PHONE_ALREADY_EXIST = "Phone already exists: %s";
    public static final String TIME_IS_UP = "Time is up";
    public static final String USED_UP_ALL = "You've used up all your attempts";
    public static final String CODE_WAS_ENTERED_INCORRECT = "The code was entered incorrectly";
    public static final String CODE_ALREADY_ENTRY = "There was already a code entry.";
    public static final String SEND_REPEAT = "Time to send a repeat code 1 minute";
    public static final String USER_NOT_FOUND = "User with this phone not found %s";
    public static final String INVALID_CATEGORY = "Invalid category: %s";
    public static final String MAX_LOAD_PHOTO = "Max load photo 8";
    public static final String INVALID_FAVORITE = "Invalid product ID %s to add, or remove to (from) favorites.";
    public static final String REVIEW_NOT_FOUND = "Review not found";
    public static final String THIS_NOT_USERS_REVIEW = "This review not write by this user";
    public static final String FAILED_RATING_UPDATE = "Failed to update product rating";
    public static final String DEFAULT_IMAGE_LINK = "https://i.pinimg.com/1200x/9f/ab/e5/9fabe5f90ca53f9a86306203f517f9fd.jpg";
    public static final String URL_IMAGE_FOR_UPLOAD = "https://testingbucket00-0-1.s3.eu-central-1.amazonaws.com/";

    private ErrorMessageHandler() {}
}
