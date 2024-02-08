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
public class UserDto {

    private String phone;

    /**
     * Compares this UserDto with the specified object for equality.
     * Two UserDto objects are considered equal if they have the same phone number.
     *
     * @param o the object to be compared for equality with this UserDto
     * @return true if the specified object is equal to this UserDto, otherwise false
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        UserDto userDto = (UserDto) o;
        return Objects.equals(phone, userDto.phone);
    }

    /**
     * Returns a hash code value for this UserDto.
     *
     * @return a hash code value for this UserDto
     */
    @Override
    public int hashCode() {
        return Objects.hash(phone);
    }
}
