package ua.marketplace.dto;

import lombok.*;

import java.util.Objects;

/**
 * A DTO (Data Transfer Object) class representing authentication information.
 */
@Builder
@Getter
@ToString
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class AuthDto {

    private UserDto user;
    private String token;

    /**
     * Compares this AuthDto with the specified object for equality.
     * Two AuthDto objects are considered equal if they have the same user and token.
     *
     * @param o the object to be compared for equality with this AuthDto
     * @return true if the specified object is equal to this AuthDto, otherwise false
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AuthDto authDto = (AuthDto) o;
        return Objects.equals(user, authDto.user) && Objects.equals(token, authDto.token);
    }

    /**
     * Returns a hash code value for this AuthDto.
     *
     * @return a hash code value for this AuthDto
     */
    @Override
    public int hashCode() {
        return Objects.hash(user, token);
    }
}
