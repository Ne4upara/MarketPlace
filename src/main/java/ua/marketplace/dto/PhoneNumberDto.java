package ua.marketplace.dto;

import lombok.*;


/**
 * A DTO (Data Transfer Object) class representing user information.
 */
@Builder
@Getter
@Setter
@ToString
@EqualsAndHashCode
@NoArgsConstructor
@AllArgsConstructor
public class PhoneNumberDto {

    private String phoneNumber;
}