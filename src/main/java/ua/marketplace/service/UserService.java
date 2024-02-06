package ua.marketplace.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import ua.marketplace.model.User;
import ua.marketplace.repository.UserRepository;

import java.util.Optional;

@Service
@RequiredArgsConstructor
public class UserService {

    private final UserRepository repository;

    public User findByUsername(String username) {
        Optional<User> user = repository.findByPhone(username);
        return user.orElse(null);
    }

    public void saveUser(User user) {
        repository.save(user);
    }
}