package ua.marketplace.requests;

import lombok.*;

/**
 * DTO representing a login request.
 */
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class LoginRequest {

    private String phone;
    private String password;

}
