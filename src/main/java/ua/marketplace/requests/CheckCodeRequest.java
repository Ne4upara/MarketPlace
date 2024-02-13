package ua.marketplace.requests;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.*;

/**
 * Request object for checking the verification code.
 */
@Getter
@Setter
@EqualsAndHashCode
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CheckCodeRequest {

    @NotEmpty(message = "Code should not be empty")
    @NotNull(message = "Code should not be null")
    @Size(min = 4, max = 4, message = "Code should be 4 symbols")
    @Pattern(regexp = "^[0-9]+$", message = "Code should contain only digits")
    private String code;
}
