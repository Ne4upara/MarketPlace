package ua.marketplace.services;

import org.springframework.stereotype.Service;
import ua.marketplace.dto.UserDto;
import ua.marketplace.entities.User;

@Service
public class UserService {

    public UserDto convertToDto(User user) {
        return UserDto
                .builder()
                .name(user.getName())
                .phoneNumber(user.getPhoneNumber())
                .build();
    }
}
