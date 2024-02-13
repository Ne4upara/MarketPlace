package ua.marketplace.dto;

import lombok.*;

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
