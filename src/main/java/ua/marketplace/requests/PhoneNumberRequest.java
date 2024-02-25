package ua.marketplace.requests;

import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.*;

/**
 * Represents an authorization request.
 */
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PhoneNumberRequest {

    /**
     * The phone number of the user.
     * Validation constraints:
     * - The phone number should contain exactly 13 digits.
     * - The phone number should be in the format +380xxxxxxxxx.
     */
    @Size(min = 13, max = 13, message = "Phone should be between 13 digits")
    @Pattern(regexp = "^\\+380\\d+$", message = "Phone should contain only digits and should be in the format +380..")
    private String phoneNumber;
}