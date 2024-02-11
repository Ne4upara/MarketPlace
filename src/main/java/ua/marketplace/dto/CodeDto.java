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
public class CodeDto {

    private String token;

    /**
     * Compares this AuthDto with the specified object for equality.
     * Two AuthDto objects are considered equal if they have the same user and token.
     *
     * @param object the object to be compared for equality with this AuthDto
     * @return true if the specified object is equal to this AuthDto, otherwise false
     */
    @Override
    public boolean equals(Object object) {
        if (this == object) return true;
        if (object == null || getClass() != object.getClass()) return false;
        CodeDto codeDto = (CodeDto) object;
        return Objects.equals(token, codeDto.token);
    }

    /**
     * Returns a hash code value for this AuthDto.
     *
     * @return a hash code value for this AuthDto
     */
    @Override
    public int hashCode() {
        return Objects.hash(token);
    }
}