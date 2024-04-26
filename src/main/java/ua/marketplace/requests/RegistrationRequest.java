package ua.marketplace.requests;

import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

/**
 * Represents a registration request.
 * This class contains two fields: `firstName` and `phoneNumber`.
 * Both fields are annotated with validation constraints to ensure that the user-provided data meets certain criteria.
 */
public record RegistrationRequest(

        // `firstName` field
        @Size(min = 2, max = 15, 
            message = "Name should be between 2 and 15 characters")
        @Pattern(regexp = "^[a-zA-Zа-яА-ЯіІїЇєЄґҐ]+$", 
            message = "Name should contain only letters (Latin or Cyrillic)")
        String firstName,

        // `phoneNumber` field
        @Size(min = 13, max = 13, 
            message = "Phone should be between 13 digits")
        @Pattern(regexp = "^\\+380\\d+$", 
            message = "Phone should contain only digits and should be in the format +380..")
        String phoneNumber) {

}
