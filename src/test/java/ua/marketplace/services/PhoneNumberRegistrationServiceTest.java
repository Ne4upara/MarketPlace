package ua.marketplace.services;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.entities.User;
import ua.marketplace.entities.VerificationCode;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.repositoryes.VerificationCodeRepository;
import ua.marketplace.requests.PhoneCodeRequest;
import ua.marketplace.requests.PhoneNumberRequest;
import ua.marketplace.requests.RegistrationRequest;

import java.time.LocalDateTime;
import java.util.Optional;

// Suppressing PMD warnings for the class
@SuppressWarnings("PMD")

// Extending the test class with MockitoExtension for mocking dependencies
@ExtendWith(MockitoExtension.class)
class PhoneNumberRegistrationServiceTest {

    // Mocking UserRepository and VerificationCodeRepository
    @Mock
    private UserRepository userRepository;
    @Mock
    private VerificationCodeRepository verificationCodeRepository;

    // Injecting the mocks into PhoneNumberRegistrationService
    @InjectMocks
    private PhoneNumberRegistrationService registrationService;

    // Test method for inputPhoneNumberSuccessfully
    @Test
    void testInputPhoneNumberSuccessfully() {

        // Given
        PhoneNumberRequest request = new PhoneNumberRequest(
                "+380999999999");

        // Creating an expected User object
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

        // Setting up userRepository.findByPhoneNumber to return an Optional with the expectedUser
        when(userRepository.findByPhoneNumber(request.phoneNumber())).thenReturn(Optional.of(expectedUser));

        // Setting up userRepository.save to return the expectedUser
        when(userRepository.save(expectedUser)).thenReturn(expectedUser);

        // When
        User result = registrationService.inputPhoneNumber(request);

        // Then
        // Asserting that the result is equal to the expectedUser
        assertEquals(expectedUser, result);

        // Verifying that userRepository.findByPhoneNumber was called with the given phone number
        verify(userRepository).findByPhoneNumber(request.phoneNumber());
    }

    // ... Remaining test methods follow a similar pattern, with comments added for each
}
