package ua.marketplace.requests;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.*;

/**
 * DTO representing a registration request.
 * Validation constraints:
 * - The phone number should not be empty, not null, and contain only digits. It should be between + and 12 digits. * -
 * It should be between 8 and 64 characters.
 */
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PhoneNumberRequest {

    @NotEmpty(message = "Phone should not be empty")
    @NotNull(message = "Phone should not be null")
    @Size(min = 13, max = 13, message = "Phone should be between 13 digits")
    @Pattern(regexp = "^\\+380\\d+$", message = "Phone should contain only digits and should be in the format +380..")
    private String phoneNumber;
}