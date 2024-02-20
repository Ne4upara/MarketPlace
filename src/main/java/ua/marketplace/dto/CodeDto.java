package ua.marketplace.dto;

import lombok.*;

/**
 * A DTO (Data Transfer Object) class representing authentication information.
 */
@Builder
@Getter
@ToString
@Setter
@EqualsAndHashCode
@NoArgsConstructor
@AllArgsConstructor
public class CodeDto {

    private String token;
    private String firstName;
}