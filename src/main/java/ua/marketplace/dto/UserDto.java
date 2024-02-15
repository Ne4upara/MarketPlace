package ua.marketplace.dto;

import lombok.*;

/**
 * Data Transfer Object (DTO) representing user details.
 */
@Getter
@Setter
@EqualsAndHashCode
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserDto {

    private String firstName;
    private String phoneNumber;
}
