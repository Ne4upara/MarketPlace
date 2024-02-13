package ua.marketplace.requests;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.*;

/**
 * Request object for user login.
 */
@Getter
@Setter
@EqualsAndHashCode
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class LoginRequest {

    @NotEmpty(message = "Phone should not be empty")
    @NotNull(message = "Phone should not be null")
    @Size(min = 9, max = 13, message = "Phone should be between 9 and 13 digits")
    @Pattern(regexp = "^[0-9+]+$", message = "Phone should contain only digits")
    private String phoneNumber;
}
