package ua.marketplace.utils;

/**
 * This class, {@code ErrorMessageHandler}, contains static final strings representing
 * error messages used throughout the application. By using these predefined error
 * messages, the application can maintain consistency and improve readability.
 *
 * The error messages cover various scenarios, such as user authorization, product
 * updates, invalid input, and more.
 */
public final class ErrorMessageHandler {
    /**
     * Error message indicating that the user is not authorized to perform a specific action.
     */
    public static final String USER_NOT_AUTHORIZED = "User not authorized";

    /**
     * Error message indicating that an attempt to update a product has failed.
     */
    public static final String FAILED_PRODUCT_UPDATE = "Failed to update product";

    /**
     * Error message indicating that the product being updated was not created by the current user.
     */
    public static final String THIS_NOT_USERS_PRODUCT = "This product was not created by this user";

    /**
     * Error message indicating that a product with the specified ID was not found.
     *
     * @param productId The ID of the product that was not found.
     */
    public static final String PRODUCT_NOT_FOUND = "Not found product with ID: %s";

    /**
     * A generic error message placeholder.
     */
    public static final String ERROR_MESSAGE = "errorMessage";

    /**
     * Error message indicating that a phone number already exists in the system.
     *
     * @param phone The phone number that already exists.
     */
    public static final String PHONE_ALREADY_EXIST = "Phone already exists: %s";

    /**
     * Error message indicating that the time limit for a specific action has been exceeded.
     */
    public static final String TIME_IS_UP = "Time is up";

    /**
     * Error message indicating that all attempts for a specific action have been used.
     */
    public static final String USED_UP_ALL = "You've used up all your attempts";

    /**
     * Error message indicating that the code entered was incorrect.
     */
    public static final String CODE_WAS_ENTERED_INCORRECT = "The code was entered incorrectly";

    /**
     * Error message indicating that there was already a code entry.
     */
    public static final String CODE_ALREADY_ENTRY = "There was already a code entry.";

    /**
     * Error message indicating that it's time to send a repeat code (1 minute).
     */
    public static final String SEND_REPEAT = "Time to send a repeat code 1 minute";

    /**
     * Error message indicating that a user with the specified phone number was not found.
     *
     * @param phone The phone number of the user that was not found.
     */
    public static final String USER_NOT_FOUND = "User with this phone not found %s";

    /**
     * Error message indicating that an invalid category was provided.
     *
     * @param category The invalid category.
     */
    public static final String INVALID_CATEGORY = "Invalid category: %s";

    /**
     * Error message indicating that the maximum number of photos (8) has been exceeded.
     */
    public static final String MAX_LOAD_PHOTO = "Max load photo 8";

    public static final String INVALID_FAVORITE = "Invalid product ID %s to add, or remove to (from) favorites.";

    /**
     * Error message indicating that a review was not found.
     */
    public static final String REVIEW_NOT_FOUND = "Review not found";

    /**
     * Error message indicating that the review being updated was not written by the current user.
     */
    public static final String THIS_NOT_USERS_REVIEW = "This review not write by this user";

    /**
     * Error message indicating that an attempt to update a product's rating has failed.
     */
    public static final String FAILED_RATING_UPDATE = "Failed to update product rating";

    /**
     * A default image link used when no image is available.
     */
    public static final String DEFAULT_IMAGE_LINK = "https://i.pinimg.com/1200x/9f/ab/e5/9fabe5f90ca53f9a86306203f517f9fd.jpg";

    // Private constructor to prevent instantiation
    private ErrorMessageHandler() {
    }

}
