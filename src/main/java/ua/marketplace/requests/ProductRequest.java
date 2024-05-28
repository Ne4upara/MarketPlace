package ua.marketplace.requests;

import jakarta.validation.constraints.*;

import java.math.BigDecimal;
import java.util.List;

/**
 * Represents a request to create a new product.
 * This record contains all the necessary information to create a new product in the system.
 */
public record ProductRequest(

        @NotBlank(message = "Product must have a name")
        @Size(min = 5, max = 100, message = "Product's name should be between 5 and 100 chars")
        String productName,

//       List<String> productPhotoLink,

        @NotNull(message = "Product must have a price")
        @Positive(message = "Product price must be positive")
        @Digits(integer = 10, fraction = 2, message = "Product price must have up to 2 decimal places")
        BigDecimal productPrice,

        @NotEmpty(message = "Product must have a description")
        @Size(min = 5, max = 250, message = "Product's description should be between 5 and 250 chars")
        String productDescription,

        @NotNull(message = "Product must have a category")
        String productCategory,

        @NotEmpty(message = "Product must have a type")
        @Pattern(regexp = "^(new|used)$", message = "Product type must be either 'new' or 'used'")
        String productType,

        @Size(min = 2, max = 15, message = "Name should be between 2 and 15 characters")
        @Pattern(regexp = "^[a-zA-Zа-яА-ЯіІїЇєЄґҐ]+$", message = "Name should contain only letters (Latin or Cyrillic)")
        String sellerName,

        @Size(min = 13, max = 13, message = "Phone should be between 13 digits")
        @Pattern(regexp = "^\\+380\\d+$", message = "Phone should contain only digits and should be in the format +380..")
        String sellerPhoneNumber,

        @Email(message = "Enter a valid email")
        String sellerEmail,

        String location
) {
}
