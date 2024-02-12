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
import ua.marketplace.dto.CodeDto;
import ua.marketplace.dto.PhoneNumberDto;
import ua.marketplace.entities.User;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.PhoneCodeRequest;
import ua.marketplace.requests.PhoneNumberRequest;
import ua.marketplace.responses.CustomResponse;
import ua.marketplace.security.JwtUtil;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Objects;
import java.util.Optional;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * Unit tests for the AuthService class.
 */
@SuppressWarnings("PMD")
@SpringBootTest
class PhoneNumberServiceTest {

    @Mock
    private UserRepository userRepository;
    @Mock
    private PasswordEncoder passwordEncoder;
    @Mock
    private JwtUtil jwtUtil;
    @InjectMocks
    private PhoneNumberRegistrationService regService;

    /**
     * Test for successful user registration.
     */
    @Test
    void testRegistrationSuccessfully() {

        //Given
        PhoneNumberRequest request = PhoneNumberRequest
                .builder()
                .phoneNumber("111111111")
                .build();

        User user = User
                .builder()
                .phone(request.getPhoneNumber())
                .build();

        when(userRepository.existsByPhone(request.getPhoneNumber())).thenReturn(false);
        when(userRepository.save(user)).thenReturn(user);

        PhoneNumberDto phoneNumberDto = PhoneNumberDto.builder().phoneNumber(user.getPhone()).build();
        ResponseEntity<CustomResponse<PhoneNumberDto>> expect = ResponseEntity.ok
                (CustomResponse.successfully(phoneNumberDto, HttpStatus.OK.value()));

        //When
        ResponseEntity<CustomResponse<PhoneNumberDto>> result = regService.inputPhoneNumber(request);

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

        ResponseEntity<CustomResponse<PhoneNumberDto>> expect = ResponseEntity
                .badRequest()
                .body(CustomResponse.failed
                        (Collections.singletonList(Error.PHONE_ALREADY_EXIST.getMessage()),
                                HttpStatus.BAD_REQUEST.value()));

        //When
        ResponseEntity<CustomResponse<PhoneNumberDto>> result = regService.inputPhoneNumber(new PhoneNumberRequest());

        //Then
        assertThat(result).isEqualTo(expect);
    }

    /**
     * Test for successful user login.
     */
    @Test
    void testLoginSuccessfully() {

        //Given
        PhoneCodeRequest request = PhoneCodeRequest
                .builder()
                .phoneNumber("+123456789012")
                .inputCode("111111")
                .build();

        User user = User
                .builder()
                .phone(request.getPhoneNumber())
                .code(request.getInputCode())
                .createdTimeCode(LocalDateTime.now())
                .build();

        CodeDto authDto = CodeDto
                .builder()
                .token("test")
                .build();

        when(userRepository.findByPhone(request.getPhoneNumber())).thenReturn(Optional.of(user));
        when(passwordEncoder.matches(any(), any())).thenReturn(true);

        //When
        ResponseEntity<CustomResponse<CodeDto>> result = regService.inputPhoneCode(request);
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
        PhoneCodeRequest request = PhoneCodeRequest
                .builder()
                .phoneNumber("+123456789012")
                .inputCode("123456")
                .build();

        when(userRepository.findByPhone(any())).thenReturn(Optional.empty());

        // When
        ResponseEntity<CustomResponse<CodeDto>> result = regService.inputPhoneCode(request);

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
        PhoneCodeRequest request = PhoneCodeRequest
                .builder()
                .phoneNumber("+123456789012")
                .inputCode("123456")
                .build();

        User user = User
                .builder()
                .phone(request.getPhoneNumber())
                .code("Invalid Code")
                .build();

        when(userRepository.findByPhone(request.getPhoneNumber())).thenReturn(Optional.of(user));
        when(passwordEncoder.matches(request.getInputCode(), user.getPassword())).thenReturn(false);

        // When
        ResponseEntity<CustomResponse<CodeDto>> result = regService.inputPhoneCode(request);

        // Then
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
        Assertions.assertTrue(Objects.requireNonNull(result
                        .getBody())
                .getMessage()
                .contains(Error.INVALID_CODE.getMessage()));
    }

    @Test
    void testLoginWithTimeIsUp() {

        //Given
        PhoneCodeRequest request = PhoneCodeRequest
                .builder()
                .phoneNumber("+123456789012")
                .inputCode("123456")
                .build();

        User user = User
                .builder()
                .phone(request.getPhoneNumber())
                .code("123456")
                .createdTimeCode(LocalDateTime.now().minusDays(1))
                .build();

        when(userRepository.findByPhone(request.getPhoneNumber())).thenReturn(Optional.of(user));
        when(passwordEncoder.matches(any(), any())).thenReturn(true);

        //When
        ResponseEntity<CustomResponse<CodeDto>> result = regService.inputPhoneCode(request);

        //Then
        Assertions.assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
        Assertions.assertTrue(Objects.requireNonNull(result
                        .getBody())
                .getMessage()
                .contains(Error.TIME_IS_UP.getMessage()));
    }
}