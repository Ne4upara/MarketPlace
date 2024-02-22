package ua.marketplace.services;

import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import ua.marketplace.entities.User;
import ua.marketplace.entities.VerificationCode;
import ua.marketplace.exception.AppException;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.repositoryes.VerificationCodeRepository;
import ua.marketplace.requests.PhoneCodeRequest;
import ua.marketplace.requests.PhoneNumberRequest;
import ua.marketplace.requests.RegistrationRequest;

import java.time.LocalDateTime;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

@SuppressWarnings("PMD")
@SpringBootTest
class PhoneNumberRegistrationServiceTest {

    @Mock
    private UserRepository userRepository;
    @Mock
    private VerificationCodeRepository verificationCodeRepository;

    @InjectMocks
    private PhoneNumberRegistrationService registrationService;

    @Test
    void testInputPhoneNumberSuccessfully() throws AppException {

        // Given
        PhoneNumberRequest request = PhoneNumberRequest.builder()
                .phoneNumber("+380999999999")
                .build();

        User expectedUser = User.builder()
                .firstName("test")
                .phoneNumber(request.getPhoneNumber())
                .verificationCode(VerificationCode.builder()
                        .code("2222")
                        .loginAttempt(0)
                        .isEntryByCode(false)
                        .createdTimeCode(LocalDateTime.now().minusMinutes(2))
                        .build())
                .isEnabled(true)
                .build();

        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.of(expectedUser));
        when(userRepository.save(expectedUser)).thenReturn(expectedUser);

        // When
        User result = registrationService.inputPhoneNumber(request);

        // Then
        assertEquals(expectedUser, result);
        verify(userRepository).findByPhoneNumber(request.getPhoneNumber());
    }

    @Test
    void testInputPhoneNumberUserDoesNotExist() {

        // Given
        PhoneNumberRequest request = PhoneNumberRequest.builder()
                .phoneNumber("+380999999999")
                .build();

        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.empty());

        // When, Then
        assertThrows(AppException.class, () -> registrationService.inputPhoneNumber(request));
        verify(userRepository).findByPhoneNumber(request.getPhoneNumber());
    }

    @Test
    void testInputPhoneNumberWithTimeToResendingCodeNotEnd() {
        // Given
        PhoneNumberRequest request = PhoneNumberRequest.builder()
                .phoneNumber("+380999999999")
                .build();

        User expectedUser = User.builder()
                .firstName("test")
                .phoneNumber(request.getPhoneNumber())
                .verificationCode(VerificationCode.builder()
                        .code("2222")
                        .loginAttempt(0)
                        .isEntryByCode(false)
                        .createdTimeCode(LocalDateTime.now())
                        .build())
                .isEnabled(true)
                .build();

        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.of(expectedUser));
        when(userRepository.save(expectedUser)).thenReturn(expectedUser);

        //When, Then
        assertThrows(AppException.class, () -> registrationService.inputPhoneNumber(request));
        verify(userRepository).findByPhoneNumber(request.getPhoneNumber());
    }

    @Test
    void testInputPhoneCodeSuccessfully() throws AppException {

        // Given
        PhoneCodeRequest request = PhoneCodeRequest.builder()
                .phoneNumber("+380999999999")
                .inputCode("2222")
                .build();

        User expectedUser = User.builder()
                .firstName("test")
                .phoneNumber(request.getPhoneNumber())
                .verificationCode(VerificationCode.builder()
                        .code("2222")
                        .loginAttempt(0)
                        .isEntryByCode(true)
                        .createdTimeCode(LocalDateTime.now().minusMinutes(2))
                        .build())
                .isEnabled(true)
                .build();

        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.of(expectedUser));

        // When
        User result = registrationService.inputPhoneCode(request);

        // Then
        assertEquals(expectedUser, result);
        verify(userRepository).findByPhoneNumber(request.getPhoneNumber());
    }

    @Test
    void testInputPhoneCodeUserDoesNotExist() {

        // Given
        PhoneCodeRequest request = PhoneCodeRequest.builder()
                .phoneNumber("+380999999999")
                .inputCode("2222")
                .build();

        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.empty());

        // When, Then
        assertThrows(AppException.class, () -> registrationService.inputPhoneCode(request));
        verify(userRepository).findByPhoneNumber(request.getPhoneNumber());
    }

    @Test
    void testInputPhoneCodeInvalidCode() {
        // Given
        PhoneCodeRequest request = PhoneCodeRequest.builder()
                .phoneNumber("+380999999999")
                .inputCode("3333")
                .build();

        User expectedUser = User.builder()
                .firstName("test")
                .phoneNumber(request.getPhoneNumber())
                .verificationCode(VerificationCode.builder()
                        .code("2222")
                        .loginAttempt(0)
                        .isEntryByCode(true)
                        .createdTimeCode(LocalDateTime.now().minusMinutes(2))
                        .build())
                .isEnabled(true)
                .build();

        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.of(expectedUser));

        // When, Then
        assertThrows(AppException.class, () -> registrationService.inputPhoneCode(request));
        verify(userRepository).findByPhoneNumber(request.getPhoneNumber());
    }

    @Test
    void testInputPhoneCodeTimeToResendCodeNotElapsed() {

        // Given
        PhoneCodeRequest request = PhoneCodeRequest.builder()
                .phoneNumber("+380999999999")
                .inputCode("2222")
                .build();

        User expectedUser = User.builder()
                .firstName("test")
                .phoneNumber(request.getPhoneNumber())
                .verificationCode(VerificationCode.builder()
                        .code("2222")
                        .loginAttempt(0)
                        .isEntryByCode(false)
                        .createdTimeCode(LocalDateTime.now())
                        .build())
                .isEnabled(true)
                .build();

        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.of(expectedUser));

        // When, Then
        assertThrows(AppException.class, () -> registrationService.inputPhoneCode(request));
        verify(userRepository).findByPhoneNumber(request.getPhoneNumber());
    }

    @Test
    void testInputPhoneCodeMaxAttemptsReached() {
        // Given
        PhoneCodeRequest request = PhoneCodeRequest.builder()
                .phoneNumber("+380999999999")
                .inputCode("222")
                .build();

        User user = User.builder()
                .verificationCode(VerificationCode.builder()
                        .code("2222")
                        .loginAttempt(3) // Max attempts reached
                        .isEntryByCode(true)
                        .createdTimeCode(LocalDateTime.now().minusMinutes(2))
                        .build())
                .build();

        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.of(user));

        // When, Then
        AppException exception = assertThrows(AppException.class, () -> registrationService.inputPhoneCode(request));
        assertEquals("You've used up all your attempts", exception.getMessage());
    }

    @Test
    void testInputPhoneCodeTimeUp() {
        // Given
        PhoneCodeRequest request = PhoneCodeRequest.builder()
                .phoneNumber("+380999999999")
                .inputCode("2222")
                .build();

        User user = User.builder()
                .verificationCode(VerificationCode.builder()
                        .code("2222")
                        .loginAttempt(0)
                        .isEntryByCode(true)
                        .createdTimeCode(LocalDateTime.now().minusMinutes(6))
                        .build())
                .build();

        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.of(user));

        // When, Then
        AppException exception = assertThrows(AppException.class, () -> registrationService.inputPhoneCode(request));
        assertEquals("Time is up", exception.getMessage());
    }

    @Test
    void testRegistrationUserSuccessfully() throws AppException {

        // Given
        RegistrationRequest request = RegistrationRequest.builder()
                .firstName("John")
                .phoneNumber("+380999999999")
                .build();

        User expectedUser = User.builder()
                .firstName(request.getFirstName())
                .phoneNumber(request.getPhoneNumber())
                .verificationCode(VerificationCode.builder()
                        .code("1111")
                        .createdTimeCode(LocalDateTime.now())
                        .isEntryByCode(false)
                        .loginAttempt(0)
                        .build())
                .build();

        when(userRepository.existsByPhoneNumber(request.getPhoneNumber())).thenReturn(false);
        when(userRepository.save(any(User.class))).thenReturn(expectedUser);

        User result = registrationService.registrationUser(request);

        assertEquals(expectedUser, result);
    }

    @Test
    void testRegistrationUser_PhoneNumberAlreadyExists() {

        // Given
        RegistrationRequest request = RegistrationRequest.builder()
                .firstName("John")
                .phoneNumber("+380999999999")
                .build();

        when(userRepository.existsByPhoneNumber(request.getPhoneNumber())).thenReturn(true);

        // When, Then
        AppException exception = assertThrows(AppException.class, () -> registrationService.registrationUser(request));
        assertEquals("Phone already exists: " + request.getPhoneNumber(), exception.getMessage());
    }
}
