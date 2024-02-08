package ua.marketplace.requests;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.*;

/**
 * DTO representing a registration request.
 * Validation constraints:
 * - The phone number should not be empty, not null, and contain only digits. It should be between 9 and 12 digits.
 * - The password should not be empty, not null, and contain only Latin letters, digits, and the following symbols: ,!.-_.
 * It should be between 8 and 64 characters.
 */
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RegisterRequest {

    @NotEmpty(message = "Phone should not be empty")
    @NotNull(message = "Phone should not be null")
    @Size(min = 9, max = 12, message = "Phone should be between 9 and 12 digits")
    @Pattern(regexp = "^\\d+$", message = "Phone should contain only digits")
    private String phone;

    @NotEmpty(message = "Password should not be empty")
    @NotNull(message = "Password should not be null")
    @Size(min = 8, max = 64, message = "Password should be between 8 and 64 characters")
    @Pattern(regexp = "^[a-zA-Z0-9,!.-_]+$",
            message = "Password should contain only Latin letters, digits, and the following symbols: ,!.-_")
    private String password;
}
