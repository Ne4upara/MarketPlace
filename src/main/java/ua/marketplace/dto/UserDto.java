package ua.marketplace.dto;

import java.util.List;

public record UserDto(
        Long id,
        String phoneNumber,
        String firstName,
        List<Long> favorite_id
) {
}
