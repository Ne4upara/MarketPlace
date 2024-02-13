package ua.marketplace.services;

import jakarta.transaction.Transactional;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import ua.marketplace.dto.UserDto;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.RegistrationRequest;
import ua.marketplace.responses.CustomResponse;
import ua.marketplace.security.JwtUtil;

import static org.mockito.Mockito.*;

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

    @Test
    @Transactional
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
}
