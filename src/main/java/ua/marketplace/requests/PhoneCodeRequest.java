package ua.marketplace.requests;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.*;

/**
 * Represents a request to enter a code for authorization.
 */
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PhoneCodeRequest {

    @NotEmpty(message = "Phone should not be empty")
    @NotNull(message = "Phone should not be null")
    @Size(min = 4, max = 4, message = "Phone should be between 4 digits")
    @Pattern(regexp = "^\\d+$", message = "Phone should contain only digits")
    private String inputCode;

    /**
     * The phone number of the user.
     * Validation constraints:
     * - The phone number should not be empty and not null.
     * - The phone number should contain exactly 13 digits.
     * - The phone number should be in the format +380xxxxxxxxx.
     */
    @NotEmpty(message = "Phone should not be empty")
    @NotNull(message = "Phone should not be null")
    @Size(min = 13, max = 13, message = "Phone should be between 13 digits")
    @Pattern(regexp = "^\\+380\\d+$", message = "Phone should contain only digits and should be in the format +380..")
    private String phoneNumber;
}