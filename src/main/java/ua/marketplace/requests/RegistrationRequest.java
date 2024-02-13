package ua.marketplace.requests;

import jakarta.persistence.Column;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.*;

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
    private String name;

    @Column(name = "phoneNumber")
    @NotEmpty(message = "Phone should not be empty")
    @NotNull(message = "Phone should not be null")
    @Size(min = 9, max = 13, message = "Phone should be between 9 and 13 digits")
    @Pattern(regexp = "^[0-9+]+$", message = "Phone should contain only digits and may start with '+'")
    private String phoneNumber;
}
