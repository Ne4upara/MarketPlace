package ua.marketplace.requests;

import jakarta.validation.constraints.*;

import java.math.BigDecimal;
import java.util.List;

/**
 * Represents a request to create a new product.
 * This record contains all the necessary information to create a new product in the system.
 */
public record ProductRequest(

        // The name of the product. It cannot be blank and should be between 5 and 100 characters long.
        @NotBlank(message = "Product must have a name")
        @Size(min = 5, max = 100, message = "Product's name should be between 5 and 100 chars")
        String productName,

        // A list of links to the product's photos.
        List<String> productPhotoLink,

        // The price of the product. It cannot be null and should be a positive number with up to 2 decimal places.
        @NotNull(message = "Product must have a price")
        @Positive(message = "Product price must be positive")
        @Digits(integer = 10, fraction = 2, message = "Product price must have up to 2 decimal places")
        BigDecimal productPrice,

        // The description of the product. It cannot be null and should be between 5 and 250 characters long.
        @NotEmpty(message = "Product must have a description")
        @Size(min = 5, max = 250, message = "Product's description should be between 5 and 250 chars")
        String productDescription,

        // The category of the product. It cannot be null.
        @NotNull(message = "Product must have a category")
        String productCategory,

        // The type of the product. It cannot be null and should be either 'new' or 'used'.
        @NotEmpty(message = "Product must have a type")
        @Pattern(regexp = "^(new|used)$", message = "Product type must be either 'new' or 'used'")
        String productType,

        // The name of the seller. It should be between 2 and 15 characters long and should contain only letters (Latin or Cyrillic).
        @Size(min = 2, max = 15, message = "Name should be between 2 and 15 characters")
        @Pattern(regexp = "^[a-zA-Zа-яА-ЯіІїЇєЄґҐ]+$", message = "Name should contain only letters (Latin or Cyrillic)")
        String sellerName,

        // The phone number of the seller. It should be in the format +380.... and should contain only digits.
        @Size(min = 13, max = 13, message = "Phone should be between 13 digits")
        @Pattern(regexp = "^\\+380\\d+$", message = "Phone should contain only digits and should be in the format +380..")
        String sellerPhoneNumber,

        // The email of the seller. It should be a valid email address.
        @Email(message = "Enter a valid email")
        String sellerEmail,

        // The location of the seller.
        String location
) {
}
