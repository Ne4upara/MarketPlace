package ua.marketplace.services;

import org.springframework.stereotype.Service;
import ua.marketplace.dto.UserDto;
import ua.marketplace.entities.User;

/**
 * Service class responsible for converting User entities to UserDto objects.
 */
@Service
public class UserService {

    /**
     * Converts a User entity to a UserDto object.
     *
     * @param user User entity to be converted.
     * @return UserDto object containing user information.
     */
    public UserDto convertToDto(User user) {
        return UserDto
                .builder()
                .firstName(user.getFirstName())
                .phoneNumber(user.getPhoneNumber())
                .build();
    }
}
