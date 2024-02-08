package ua.marketplace.services;

import org.springframework.stereotype.Service;
import ua.marketplace.dto.UserDto;
import ua.marketplace.entitys.User;

/**
 * A service class for handling user-related operations.
 */
@Service
public class UserService {

    /**
     * Converts a User object to a UserDto object.
     *
     * @param user the User object to convert
     * @return the UserDto object converted from the User
     */
    public UserDto convertToDto(User user) {
        return UserDto
                .builder()
                .phone(user.getPhone())
                .build();
    }
}
