package ua.marketplace.security;

import lombok.RequiredArgsConstructor;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import ua.marketplace.entities.User;
import ua.marketplace.repositoryes.UserRepository;

import java.util.Collections;

/**
 * A custom implementation of the Spring Security UserDetailsService interface.
 * This service is responsible for loading user details from the database based on the phoneNumber number.
 */
@Service
@RequiredArgsConstructor
public class CustomUserDetailsService implements UserDetailsService {

    private final UserRepository userRepository;

    /**
     * Loads the user details from the database based on the provided phoneNumber number.
     *
     * @param phone the phoneNumber number of the user to load
     * @return a UserDetails object representing the loaded user
     * @throws UsernameNotFoundException if no user with the specified phoneNumber number is found
     */
    @Override
    public UserDetails loadUserByUsername(String phone) throws UsernameNotFoundException {

        User user = userRepository.findByPhoneNumber(phone)
                .orElseThrow(() -> new UsernameNotFoundException("User not found: " + phone));

        return new org.springframework.security.core.userdetails.User(
                user.getPhoneNumber(),
                user.getName(),
                Collections.emptyList());
    }
}