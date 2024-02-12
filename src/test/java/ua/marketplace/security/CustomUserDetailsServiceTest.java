package ua.marketplace.security;


import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import ua.marketplace.entitys.User;
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
                .phone("1111111111")
                .password("test1234")
                .build();

        when(repository.findByPhone(any())).thenReturn(Optional.of(user));

        UserDetails expect = org.springframework.security.core.userdetails.User.builder()
                .username(user.getPhone())
                .password(user.getPassword())
                .build();

        //When
        UserDetails result = service.loadUserByUsername(user.getPhone());

        //Then
        Assertions.assertEquals(expect, result);
    }

    /**
     * Testing the loadUserByUsername method when the user is not found.
     */
    @Test
    void testLoadUserByUsernameWithUserNotFound() {

        // Given
        when(repository.findByPhone(any())).thenReturn(Optional.empty());

        // When/Then
        assertThrows(UsernameNotFoundException.class, () -> service.loadUserByUsername("test"));
    }

}