package ua.marketplace.constants;

public final class ErrorMessage {
    public static final String USER_NOT_AUTHORIZED = "User not authorized";
    public static final String FAILED_PRODUCT_UPDATE = "Failed to update product";
    public static final String PRODUCT_NOT_FOUND = "Not found product with ID: ";
    public static final String PRODUCT_RATING_ERROR = "Rating must be between 0 and 5";
    public static final String DELETING_WITH_NOT_AUTHORIZED_USER = "You are not authorized to delete this product";

    private ErrorMessage(){}
}
