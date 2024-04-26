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
 * A custom implementation of the Spring Security UserDetailsService interface.  <-- We implement UserDetailsService to load user details from the database.
 * This service is responsible for loading user details from the database based on the phone number.  <-- The loadUserByUsername method is overridden to load user details using the UserRepository.
 */
@Service
@RequiredArgsConstructor
public class CustomUserDetailsService implements UserDetailsService {

    private final UserRepository userRepository;  <-- UserRepository is autowired to load user details from the database.

    /**
     * Loads the user details from the database based on the provided phone number.  <-- This method is called by Spring Security to load user details for authentication.
     *
     * @param phone the phone number of the user to load
     * @return a UserDetails object representing the loaded user
     * @throws UsernameNotFoundException if no user with the specified phone number is found
     */
    @Override
    public UserDetails loadUserByUsername(String phone) throws UsernameNotFoundException {

        User user = userRepository.findByPhoneNumber(phone)
                .orElseThrow(() -> new UsernameNotFoundException("User not found: " + phone));  <-- This line loads the user details from the database based on the provided phone number.

        return new org.springframework.security.core.userdetails.User(
                user.getPhoneNumber(),  <-- The UserDetails object is created using the user's phone number, first name, and an empty list of authorities.
                user.getFirstName(),
                Collections.emptyList());
    }
}
