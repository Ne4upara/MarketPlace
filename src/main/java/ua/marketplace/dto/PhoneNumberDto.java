package ua.marketplace.dto;

import lombok.*;

import java.util.Objects;

/**
 * A DTO (Data Transfer Object) class representing user information.
 */
@Builder
@Getter
@Setter
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class PhoneNumberDto {

    private String phoneNumber;

    /**
     * Compares this UserDto with the specified object for equality.
     * Two UserDto objects are considered equal if they have the same phone number.
     *
     * @param object the object to be compared for equality with this UserDto
     * @return true if the specified object is equal to this UserDto, otherwise false
     */
    @Override
    public boolean equals(Object object) {
        if (this == object) return true;
        if (object == null || getClass() != object.getClass()) return false;
        PhoneNumberDto phoneNumberDto = (PhoneNumberDto) object;
        return Objects.equals(phoneNumber, phoneNumberDto.phoneNumber);
    }

    /**
     * Returns a hash code value for this UserDto.
     *
     * @return a hash code value for this UserDto
     */
    @Override
    public int hashCode() {
        return Objects.hash(phoneNumber);
    }
}