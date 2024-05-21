package ua.marketplace.security;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.test.context.TestPropertySource;
import ua.marketplace.BaseTest;
import ua.marketplace.repositoryes.BlackListRepository;

import static org.mockito.Mockito.when;

/**
 * Unit testing for the JwtUtil class.
 */

@SuppressWarnings("PMD")

class JwtUtilTest extends BaseTest {

    @Mock
    private UserDetailsService service;

    @Mock
    private BlackListRepository blackListRepository;
    @InjectMocks
    private JwtUtil jwtUtil;

    /**
     * Testing token generation.
     */
    @Test
    void testGenerateToken() {

        //Given
        UserDetails userDetails = org.springframework.security.core.userdetails.User.builder()
                .username("Test111")
                .password("test1111")
                .build();
        when(service.loadUserByUsername(userDetails.getUsername())).thenReturn(userDetails);

        //When
        String token = jwtUtil.generateToken(userDetails.getUsername());

        //Then
        Assertions.assertNotNull(token);
        Assertions.assertFalse(token.isEmpty());
    }

    /**
     * Testing token validation with a valid token.
     */
    @Test
    void testValidateTokenWithValidToken() {

        //Given
        UserDetails userDetails = org.springframework.security.core.userdetails.User.builder()
                .username("Test111")
                .password("test1111")
                .build();

        when(service.loadUserByUsername(userDetails.getUsername())).thenReturn(userDetails);
        String token = jwtUtil.generateToken(userDetails.getUsername());

        //When
        boolean isValid = jwtUtil.validateToken(token, userDetails);

        //Then
        Assertions.assertTrue(isValid);
    }

}