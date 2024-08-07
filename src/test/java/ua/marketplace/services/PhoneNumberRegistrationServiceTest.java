package ua.marketplace.services;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.annotation.Rollback;
import org.springframework.test.context.TestPropertySource;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.entities.User;
import ua.marketplace.entities.VerificationCode;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.repositoryes.VerificationCodeRepository;
import ua.marketplace.requests.PhoneCodeRequest;
import ua.marketplace.requests.PhoneNumberRequest;
import ua.marketplace.requests.RegistrationRequest;
import ua.marketplace.services.impl.PhoneNumberRegistrationServiceImpl;

import java.time.LocalDateTime;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

@SuppressWarnings("PMD")
@ExtendWith(MockitoExtension.class)
@TestPropertySource(locations="classpath:application-dev.properties")
@Transactional
class PhoneNumberRegistrationServiceTest {

    @Mock
    private UserRepository userRepository;
    @Mock
    private VerificationCodeRepository verificationCodeRepository;
    @InjectMocks
    private PhoneNumberRegistrationServiceImpl registrationService;

    @Test
    @Rollback
    void testInputPhoneNumberSuccessfully() {

        // Given
        PhoneNumberRequest request = new PhoneNumberRequest(
                "+380999999999");

        User expectedUser = User.builder()
                .firstName("test")
                .phoneNumber(request.phoneNumber())
                .verificationCode(VerificationCode.builder()
                        .code("2222")
                        .loginAttempt(0)
                        .isEntryByCode(false)
                        .createdTimeCode(LocalDateTime.now().minusMinutes(2))
                        .build())
                .isEnabled(true)
                .build();

        when(userRepository.findByPhoneNumber(request.phoneNumber())).thenReturn(Optional.of(expectedUser));
        when(userRepository.save(expectedUser)).thenReturn(expectedUser);

        // When
        User result = registrationService.inputPhoneNumber(request);

        // Then
        assertEquals(expectedUser, result);
        verify(userRepository).findByPhoneNumber(request.phoneNumber());
    }

    @Test
    @Rollback
    void testInputPhoneNumberUserDoesNotExist() {

        // Given
        PhoneNumberRequest request = new PhoneNumberRequest(
                "+380999999999");

        when(userRepository.findByPhoneNumber(request.phoneNumber())).thenReturn(Optional.empty());

        // When, Then
        assertThrows(ResponseStatusException.class, () -> registrationService.inputPhoneNumber(request));
        verify(userRepository).findByPhoneNumber(request.phoneNumber());
    }

    @Test
    @Rollback
    void testInputPhoneNumberWithTimeToResendingCodeNotEnd() {
        // Given
        PhoneNumberRequest request = new PhoneNumberRequest(
                "+380999999999");

        User expectedUser = User.builder()
                .firstName("test")
                .phoneNumber(request.phoneNumber())
                .verificationCode(VerificationCode.builder()
                        .code("2222")
                        .loginAttempt(0)
                        .isEntryByCode(false)
                        .createdTimeCode(LocalDateTime.now())
                        .build())
                .isEnabled(true)
                .build();

        when(userRepository.findByPhoneNumber(request.phoneNumber())).thenReturn(Optional.of(expectedUser));

        //When, Then
        assertThrows(ResponseStatusException.class, () -> registrationService.inputPhoneNumber(request));
        verify(userRepository).findByPhoneNumber(request.phoneNumber());
    }

    @Test
    @Rollback
    void testInputPhoneCodeSuccessfully() {

        // Given
        PhoneCodeRequest request = new PhoneCodeRequest(
                "2222",
                "+380999999999");

        User expectedUser = User.builder()
                .firstName("test")
                .phoneNumber(request.phoneNumber())
                .verificationCode(VerificationCode.builder()
                        .code("2222")
                        .loginAttempt(0)
                        .isEntryByCode(true)
                        .createdTimeCode(LocalDateTime.now().minusMinutes(2))
                        .build())
                .isEnabled(true)
                .build();

        when(userRepository.findByPhoneNumber(request.phoneNumber())).thenReturn(Optional.of(expectedUser));

        // When
        User result = registrationService.inputPhoneCode(request);

        // Then
        assertEquals(expectedUser, result);
        verify(userRepository).findByPhoneNumber(request.phoneNumber());
    }

    @Test
    @Rollback
    void testInputPhoneCodeUserDoesNotExist() {

        // Given
        PhoneCodeRequest request = new PhoneCodeRequest(
                "2222",
                "+380999999999");

        when(userRepository.findByPhoneNumber(request.phoneNumber())).thenReturn(Optional.empty());

        // When, Then
        assertThrows(ResponseStatusException.class, () -> registrationService.inputPhoneCode(request));
        verify(userRepository).findByPhoneNumber(request.phoneNumber());
    }

    @Test
    @Rollback
    void testInputPhoneCodeInvalidCode() {
        // Given
        PhoneCodeRequest request = new PhoneCodeRequest(
                "3333",
                "+380999999999");

        User expectedUser = User.builder()
                .firstName("test")
                .phoneNumber(request.phoneNumber())
                .verificationCode(VerificationCode.builder()
                        .code("2222")
                        .loginAttempt(0)
                        .isEntryByCode(true)
                        .createdTimeCode(LocalDateTime.now().minusMinutes(2))
                        .build())
                .isEnabled(true)
                .build();

        when(userRepository.findByPhoneNumber(request.phoneNumber())).thenReturn(Optional.of(expectedUser));

        // When, Then
        assertThrows(ResponseStatusException.class, () -> registrationService.inputPhoneCode(request));
        verify(userRepository).findByPhoneNumber(request.phoneNumber());
    }

    @Test
    @Rollback
    void testInputPhoneCodeTimeToResendCodeNotElapsed() {

        // Given
        PhoneCodeRequest request = new PhoneCodeRequest(
                "2222",
                "+380999999999");

        User expectedUser = User.builder()
                .firstName("test")
                .phoneNumber(request.phoneNumber())
                .verificationCode(VerificationCode.builder()
                        .code("2222")
                        .loginAttempt(0)
                        .isEntryByCode(false)
                        .createdTimeCode(LocalDateTime.now())
                        .build())
                .isEnabled(true)
                .build();

        when(userRepository.findByPhoneNumber(request.phoneNumber())).thenReturn(Optional.of(expectedUser));

        // When, Then
        assertThrows(ResponseStatusException.class, () -> registrationService.inputPhoneCode(request));
        verify(userRepository).findByPhoneNumber(request.phoneNumber());
    }

    @Test
    @Rollback
    void testInputPhoneCodeMaxAttemptsReached() {
        // Given
        PhoneCodeRequest request = new PhoneCodeRequest(
                "222",
                "+380999999999");

        User user = User.builder()
                .verificationCode(VerificationCode.builder()
                        .code("2222")
                        .loginAttempt(3) // Max attempts reached
                        .isEntryByCode(true)
                        .createdTimeCode(LocalDateTime.now().minusMinutes(2))
                        .build())
                .build();

        when(userRepository.findByPhoneNumber(request.phoneNumber())).thenReturn(Optional.of(user));

        // When, Then
        ResponseStatusException exception = assertThrows(ResponseStatusException.class, () -> registrationService.inputPhoneCode(request));
        assertEquals("409 CONFLICT \"You've used up all your attempts\"", exception.getMessage());
    }

    @Test
    @Rollback
    void testInputPhoneCodeTimeUp() {
        // Given
        PhoneCodeRequest request = new PhoneCodeRequest(
                "2222",
                "+380999999999");

        User user = User.builder()
                .verificationCode(VerificationCode.builder()
                        .code("2222")
                        .loginAttempt(0)
                        .isEntryByCode(true)
                        .createdTimeCode(LocalDateTime.now().minusMinutes(6))
                        .build())
                .build();

        when(userRepository.findByPhoneNumber(request.phoneNumber())).thenReturn(Optional.of(user));

        // When, Then
        ResponseStatusException exception = assertThrows(ResponseStatusException.class, () -> registrationService.inputPhoneCode(request));
        assertEquals("409 CONFLICT \"Time is up\"", exception.getMessage());
    }

    @Test
    @Rollback
    void testRegistrationUserSuccessfully() {

        // Given
        RegistrationRequest request = new RegistrationRequest(
                "John",
                "+380999999999");

        User expectedUser = User.builder()
                .firstName(request.firstName())
                .phoneNumber(request.phoneNumber())
                .verificationCode(VerificationCode.builder()
                        .code("1111")
                        .createdTimeCode(LocalDateTime.now())
                        .isEntryByCode(false)
                        .loginAttempt(0)
                        .build())
                .build();

        when(userRepository.existsByPhoneNumber(request.phoneNumber())).thenReturn(false);
        when(userRepository.save(any(User.class))).thenReturn(expectedUser);

        User result = registrationService.registration(request);

        assertEquals(expectedUser, result);
    }

    @Test
    @Rollback
    void testRegistrationUser_PhoneNumberAlreadyExists() {

        // Given
        RegistrationRequest request = new RegistrationRequest(
                "John",
                "+380999999999");

        when(userRepository.existsByPhoneNumber(request.phoneNumber())).thenReturn(true);

        // When, Then
        ResponseStatusException exception = assertThrows
                (ResponseStatusException.class, () -> registrationService.registration(request));
        assertEquals("409 CONFLICT \"Phone already exists: " + request.phoneNumber() + "\"",
                exception.getMessage());
    }
}