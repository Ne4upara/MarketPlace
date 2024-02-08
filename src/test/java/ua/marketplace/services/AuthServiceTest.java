package ua.marketplace.services;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.password.PasswordEncoder;
import ua.marketplace.data.Error;
import ua.marketplace.dto.AuthDto;
import ua.marketplace.dto.UserDto;
import ua.marketplace.entitys.User;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.LoginRequest;
import ua.marketplace.requests.RegisterRequest;
import ua.marketplace.responses.CustomResponse;
import ua.marketplace.security.JwtUtil;

import java.util.Collections;
import java.util.Objects;
import java.util.Optional;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * Unit tests for the AuthService class.
 */
@SpringBootTest
class AuthServiceTest {

    @Mock
    private JwtUtil jwtUtil;
    @Mock
    private UserRepository userRepository;
    @Mock
    private PasswordEncoder passwordEncoder;
    @Mock
    private UserService userService;
    @InjectMocks
    private AuthService authService;

    /**
     * Test for successful user registration.
     */
    @Test
    void testRegistrationSuccessfully() {

        //Given
        RegisterRequest request = RegisterRequest
                .builder()
                .phone("111111111")
                .password("test1111")
                .build();

        User user = User
                .builder()
                .phone(request.getPhone())
                .password(request.getPassword())
                .build();

        when(userRepository.existsByPhone(request.getPhone())).thenReturn(false);
        when(userRepository.save(user)).thenReturn(user);

        ResponseEntity<CustomResponse<UserDto>> expect = ResponseEntity.ok
                (CustomResponse.successfully(userService.convertToDto(user), HttpStatus.OK.value()));

        //When
        ResponseEntity<CustomResponse<UserDto>> result = authService.registration(request);

        //Then
        assertThat(result).isEqualTo(expect);
    }

    /**
     * Test for user registration with an existing phone number.
     */
    @Test
    void testRegistrationWithPhoneExist() {

        //Given
        when(userRepository.existsByPhone(any())).thenReturn(true);

        ResponseEntity<CustomResponse<UserDto>> expect = ResponseEntity
                .badRequest()
                .body(CustomResponse.failed
                        (Collections.singletonList(Error.PHONE_ALREADY_EXIST.getMessage()),
                                HttpStatus.BAD_REQUEST.value()));

        //When
        ResponseEntity<CustomResponse<UserDto>> result = authService.registration(new RegisterRequest());

        //Then
        assertThat(result).isEqualTo(expect);
    }

    /**
     * Test for successful user login.
     */
    @Test
    void testLoginSuccessfully() {

        //Given
        LoginRequest request = LoginRequest
                .builder()
                .phone("111111111")
                .password("test1111")
                .build();

        User user = User
                .builder()
                .phone(request.getPhone())
                .password(request.getPassword())
                .build();

        AuthDto authDto = AuthDto
                .builder()
                .user(userService.convertToDto(user))
                .build();

        when(userRepository.findByPhone(request.getPhone())).thenReturn(Optional.of(user));
        when(passwordEncoder.matches(any(), any())).thenReturn(true);

        //When
        ResponseEntity<CustomResponse<AuthDto>> result = authService.login(request);
        authDto.setToken(Objects.requireNonNull(result.getBody()).getBody().getToken());

        //Then
        assertThat(result.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(result.getBody()).isNotNull();
        assertThat(result.getBody().getBody()).isEqualTo(authDto);
    }

    /**
     * Test for user login with user not found.
     */
    @Test
    void testLoginWithUserNotFound() {

        //Given
        LoginRequest request = LoginRequest
                .builder()
                .phone("111111111")
                .password("test1111")
                .build();

        when(userRepository.findByPhone(any())).thenReturn(Optional.empty());

        // When
        ResponseEntity<CustomResponse<AuthDto>> result = authService.login(request);

        // Then
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
        Assertions.assertTrue(Objects.requireNonNull(result.getBody()).getMessage().contains(Error.USER_NOT_FOUND.getMessage()));
    }

    /**
     * Test for user login with an invalid password.
     */
    @Test
    void testLoginWithInvalidPassword() {

        //Given
        LoginRequest request = LoginRequest
                .builder()
                .phone("111111111")
                .password("test1111")
                .build();

        User user = User
                .builder()
                .phone(request.getPhone())
                .password("Invalid Password")
                .build();

        when(userRepository.findByPhone(request.getPhone())).thenReturn(Optional.of(user));
        when(passwordEncoder.matches(request.getPassword(), user.getPassword())).thenReturn(false);

        // When
        ResponseEntity<CustomResponse<AuthDto>> result = authService.login(request);

        // Then
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
        Assertions.assertTrue(Objects.requireNonNull(result.getBody()).getMessage().contains(Error.INVALID_PASSWORD.getMessage()));
    }
}