package ua.marketplace.requests;

import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

/**
 * Represents an authorization request.
 */
public record PhoneNumberRequest(
        @Size(min = 13, max = 13, message = "Phone should be between 13 digits")
        @Pattern(regexp = "^\\+380\\d+$", message = "Phone should contain only digits and should be in the format +380..")
        String phoneNumber) {
}
