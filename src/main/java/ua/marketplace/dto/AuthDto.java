package ua.marketplace.dto;

import lombok.*;

/**
 * Data Transfer Object (DTO) representing authentication details.
 */
@Getter
@Setter
@EqualsAndHashCode
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AuthDto {

    private UserDto user;
    private String token;
}
