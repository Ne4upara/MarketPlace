package ua.marketplace.requests;

import jakarta.validation.constraints.*;
import ua.marketplace.data.ProductCategory;

import java.math.BigDecimal;

/**
 * Represents a request to create a product.
 */
public record ProductRequest(
        @NotBlank(message = "Product must have a name")
        @Size(min = 5, max = 100, message = "Product's name should be between 5 and 100 chars")
        String productName,

        String productPhotoLink,

        @NotNull(message = "Product must have a price")
        @Positive(message = "Product price must be positive")
        @Digits(integer = 10, fraction = 2, message = "Product price must have up to 2 decimal places")
        BigDecimal productPrice,

        @NotEmpty(message = "Product must have a description")
        @Size(min = 10, max = 250, message = "Product's description should be between 5 and 250 chars")
        @Pattern(regexp = "^[а-яА-Я0-9\\s]*$",
                message = "Description should contain only Cyrillic characters, spaces, and digits")
        String productDescription,

        @NotNull(message = "You must choice a category for your product's")
        ProductCategory productCategory,

        @NotEmpty(message = "You must choice a type of your product's")
        @Pattern(regexp = "^(new|used)$", message = "Product type must be either 'new' or 'used'")
        String productType,

        @NotNull(message = "Product quantity must be specified")
        @Positive(message = "Product quantity must be positive")
        int productQuantity
) {
}