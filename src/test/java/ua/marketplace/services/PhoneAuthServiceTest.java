package ua.marketplace.services;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import ua.marketplace.data.Error;
import ua.marketplace.dto.AuthDto;
import ua.marketplace.dto.UserDto;
import ua.marketplace.entities.User;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.CheckCodeRequest;
import ua.marketplace.requests.LoginRequest;
import ua.marketplace.requests.RegistrationRequest;
import ua.marketplace.responses.CustomResponse;
import ua.marketplace.security.JwtUtil;

import java.util.Collections;
import java.util.Objects;
import java.util.Optional;

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.when;

/**
 * Unit tests for {@link PhoneAuthService}.
 */
@SuppressWarnings("PMD")
@SpringBootTest
class PhoneAuthServiceTest {

    @Mock
    private UserRepository userRepository;
    @Mock
    private UserService userService;
    @Mock
    private CodeService codeService;
    @Mock
    private JwtUtil jwtUtil;
    @InjectMocks
    private PhoneAuthService phoneAuthService;

    /**
     * Test for successful user registration.
     */
    @Test
    void testRegistrationSuccessfully() {

        // Given
        RegistrationRequest request = RegistrationRequest
                .builder()
                .name("Test")
                .phoneNumber("999999999")
                .build();

        when(userRepository.existsByPhoneNumber(request.getPhoneNumber())).thenReturn(false);
        ResponseEntity<CustomResponse<UserDto>> expect = ResponseEntity
                .ok()
                .body(CustomResponse.successfully(null,
                        HttpStatus.OK.value()));

        //When
        ResponseEntity<CustomResponse<UserDto>> result = phoneAuthService.registration(request);

        //Then
        Assertions.assertEquals(expect, result);
    }

    /**
     * Test for registration with an existing phone number.
     */
    @Test
    void testRegistrationWithExistPhone() {

        //Given
        when(userRepository.existsByPhoneNumber(any())).thenReturn(true);
        ResponseEntity<CustomResponse<UserDto>> expect = ResponseEntity
                .badRequest()
                .body(CustomResponse.failed(Collections.singletonList(Error.PHONE_ALREADY_EXIST.getMessage()),
                        HttpStatus.BAD_REQUEST.value()));

        //When
        ResponseEntity<CustomResponse<UserDto>> result = phoneAuthService.registration(new RegistrationRequest());

        //Then
        Assertions.assertEquals(expect, result);
    }

    /**
     * Test for successful user login.
     */
    @Test
    void testLoginSuccessfully() {

        //Given
        LoginRequest request = LoginRequest
                .builder()
                .phoneNumber("999999999")
                .build();

        User user = User
                .builder()
                .phoneNumber(request.getPhoneNumber())
                .name("Test")
                .build();

        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.ofNullable(user));
        ResponseEntity<CustomResponse<UserDto>> expect = ResponseEntity
                .ok()
                .body(CustomResponse.successfully(userService.convertToDto(Objects.requireNonNull(user)),
                        HttpStatus.OK.value()));

        //When
        ResponseEntity<CustomResponse<UserDto>> result = phoneAuthService.login(request);

        //Then
        Assertions.assertEquals(expect, result);
    }

    /**
     * Test for login with a user not found.
     */
    @Test
    void testLoginWithUserNotFound() {

        //Given
        when(userRepository.findByPhoneNumber(any())).thenReturn(Optional.empty());
        ResponseEntity<CustomResponse<UserDto>> expect = ResponseEntity
                .badRequest()
                .body(CustomResponse.failed(Collections.singletonList(Error.USER_NOT_FOUND.getMessage()),
                        HttpStatus.BAD_REQUEST.value()));

        //When
        ResponseEntity<CustomResponse<UserDto>> result = phoneAuthService.login(new LoginRequest());

        //Then
        Assertions.assertEquals(expect, result);
    }

    /**
     * Test for successful code verification.
     */
    @Test
    void testCheckCodeSuccessfully() {

        //Given
        CheckCodeRequest request = CheckCodeRequest
                .builder()
                .code("1111")
                .build();

        User user = User
                .builder()
                .phoneNumber("999999999")
                .name("Test")
                .code("1111")
                .build();

        AuthDto authDto = AuthDto
                .builder()
                .token(jwtUtil.generateToken(user.getPhoneNumber()))
                .user(userService.convertToDto(user))
                .build();

        when(userRepository.findByCode(request.getCode())).thenReturn(Optional.of(user));
        ResponseEntity<CustomResponse<AuthDto>> expect = ResponseEntity
                .ok()
                .body(CustomResponse.successfully(authDto,
                        HttpStatus.OK.value()));

        //When
        ResponseEntity<CustomResponse<AuthDto>> result = phoneAuthService.checkCode(request);

        //Then
        Assertions.assertEquals(expect, result);
    }

    /**
     * Test for code verification with a user not found.
     */
    @Test
    void testCheckCodeWithUserNotFound() {

        //Given
        when(userRepository.findByCode(any())).thenReturn(Optional.empty());
        ResponseEntity<CustomResponse<Object>> expect = ResponseEntity
                .badRequest()
                .body(CustomResponse.failed(Collections.singletonList(Error.USER_NOT_FOUND.getMessage()),
                        HttpStatus.BAD_REQUEST.value()));

        //When
        ResponseEntity<CustomResponse<AuthDto>> result = phoneAuthService.checkCode(new CheckCodeRequest());

        //Then
        Assertions.assertEquals(expect, result);
    }
}
