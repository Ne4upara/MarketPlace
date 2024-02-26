package ua.marketplace.requests;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;
import lombok.*;

import java.math.BigDecimal;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProductRequest {

    @NotEmpty(message = "Product must have a name")
    @Size(min = 5, max = 100, message = "Product's name should be between 5 and 100 chars")
    private String productName;

    private String productPhotoLink;

    @NotEmpty(message = "Product must have a price")
    @Positive(message = "Product price must be positive")
    @Pattern(regexp = "^\\d+(\\.\\d{1,2})?$", message = "Price must have up to 2 decimal places")
    private BigDecimal productPrice;

    @NotEmpty(message = "Product must have a description")
    @Size(min = 10, max = 100, message = "Product's description should be between 5 and 100 chars")
    @Pattern(regexp = "^[а-яА-Я0-9\\s]*$",
            message = "Description should contain only Cyrillic characters, spaces, and digits")
    private String productDescription;

    @NotEmpty(message = "You must choice a category for your product's")
    private String productCategory;

    @NotEmpty(message = "You must choice a type of your product's")
    @Pattern(regexp = "^(new|used)$", message = "Product type must be either 'new' or 'used'")
    private String productType;

    @NotEmpty(message = "Product quantity must be specified")
    @Positive(message = "Product quantity must be positive")
    private int productQuantity;
}
