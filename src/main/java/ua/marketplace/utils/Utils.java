package ua.marketplace.utils;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.entities.User;
import ua.marketplace.repositoryes.UserRepository;

import java.security.Principal;
import java.util.UUID;

@Component
//@Service
@RequiredArgsConstructor
public class Utils {

    private final UserRepository userRepository;

    public String getRandomString(){
        UUID uuid = UUID.randomUUID();
        return uuid.toString().replace("-", "");
    }

    public User getUserByPrincipal(Principal principal) {
        return userRepository.findByPhoneNumber(principal.getName())
                .orElseThrow(() -> new ResponseStatusException
                        (HttpStatus.UNAUTHORIZED, ErrorMessageHandler.USER_NOT_AUTHORIZED));
    }
}
