package ua.marketplace.requests;

import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;
import lombok.*;

/**
 * Represents a registration request.
 */
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RegistrationRequest {

    /**
     * The first name of the user.
     * Validation constraints:
     * - The username must contain exactly a minimum of 2, maximum of 15 characters.
     * - The phone number should contain only letters Latin or Cyrillic.
     */
    @Size(min = 2, max = 15, message = "Name should be between 2 and 15 characters")
    @Pattern(regexp = "^[a-zA-Zа-яА-ЯіІїЇєЄґҐ]+$", message = "Name should contain only letters (Latin or Cyrillic)")
    private String firstName;
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
