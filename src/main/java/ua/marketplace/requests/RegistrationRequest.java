package ua.marketplace.requests;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.*;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RegistrationRequest {

    @NotEmpty(message = "Name should not be empty")
    @NotNull(message = "Name should not be null")
    @Size(min = 2, max = 15, message = "Name should be between 2 and 15 characters")
    @Pattern(regexp = "^[a-zA-Zа-яА-ЯіІїЇєЄґҐ]+$", message = "Name should contain only letters (Latin or Cyrillic)")
    private String firstName;

    @NotEmpty(message = "Phone should not be empty")
    @NotNull(message = "Phone should not be null")
    @Size(min = 13, max = 13, message = "Phone should be between 13 digits")
    @Pattern(regexp = "^\\+380\\d+$", message = "Phone should contain only digits and should be in the format +380..")
    private String phoneNumber;
}
