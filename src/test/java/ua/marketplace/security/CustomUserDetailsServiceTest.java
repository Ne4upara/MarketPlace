package ua.marketplace.security;


import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import ua.marketplace.entities.User;
import ua.marketplace.repositoryes.UserRepository;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * Unit testing for the CustomUserDetailsService class.
 */
@SpringBootTest
class CustomUserDetailsServiceTest {

    @InjectMocks
    CustomUserDetailsService service;
    @Mock
    private UserRepository repository;

    /**
     * Testing the loadUserByUsername method with a successful scenario.
     */
    @Test
    void testLoadUserByUsernameSuccessfully() {

        //Given
        User user = User.builder()
                .phoneNumber("1111111111")
                .firstName("Test")
                .build();

        when(repository.findByPhoneNumber(any())).thenReturn(Optional.of(user));

        UserDetails expect = org.springframework.security.core.userdetails.User.builder()
                .username(user.getPhoneNumber())
                .password(user.getFirstName())
                .build();

        //When
        UserDetails result = service.loadUserByUsername(user.getPhoneNumber());

        //Then
        Assertions.assertEquals(expect, result);
    }

    /**
     * Testing the loadUserByUsername method when the user is not found.
     */
    @Test
    void testLoadUserByUsernameWithUserNotFound() {

        // Given
        when(repository.findByPhoneNumber(any())).thenReturn(Optional.empty());

        // When/Then
        assertThrows(UsernameNotFoundException.class, () -> service.loadUserByUsername("test"));
    }

}