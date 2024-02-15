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
    @Size(min = 17, max = 17, message = "Phone should be 16 characters")
    @Pattern(regexp = "^\\+38\\(0\\d{2}\\)\\d{3}-\\d{2}-\\d{2}$",
            message = "Phone should be in the format +38(0**)***-**-**")
    private String phoneNumber;
}
