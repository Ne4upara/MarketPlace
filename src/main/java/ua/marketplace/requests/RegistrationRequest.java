package ua.marketplace.requests;

import jakarta.persistence.Column;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.*;

/**
 * Request object for user registerUser.
 */
@Getter
@Setter
@EqualsAndHashCode
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RegistrationRequest {

    @NotEmpty(message = "Name should not be empty")
    @NotNull(message = "Name should not be null")
    @Size(min = 2, max = 20, message = "Name should be between 2 and 20 characters")
    @Pattern(regexp = "^[a-zA-Zа-яА-ЯіІїЇєЄґҐ]+$", message = "Name should contain only letters (Latin or Cyrillic)")
    private String firstName;

    @Column(name = "phoneNumber")
    @NotEmpty(message = "Phone should not be empty")
    @NotNull(message = "Phone should not be null")
    @Size(min = 17, max = 17, message = "Phone should be 16 characters")
    @Pattern(regexp = "^\\+38\\(0\\d{2}\\)\\d{3}-\\d{2}-\\d{2}$",
            message = "Phone should be in the format +38(0**)***-**-**")
    private String phoneNumber;
}
