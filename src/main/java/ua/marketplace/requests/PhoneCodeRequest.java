package ua.marketplace.requests;

import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

/**
 * Represents a request to enter a code for authorization.
 */
public record PhoneCodeRequest(
        @Size(min = 4, max = 4, message = "Phone should be between 4 digits")
        @Pattern(regexp = "^\\d+$", message = "Phone should contain only digits")
        String inputCode,


        @Size(min = 13, max = 13, message = "Phone should be between 13 digits")
        @Pattern(regexp = "^\\+380\\d+$", message = "Phone should contain only digits and should be in the format +380..")
        String phoneNumber) {
}