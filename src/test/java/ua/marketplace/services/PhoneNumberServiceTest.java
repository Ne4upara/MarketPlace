//package ua.marketplace.services;
//
//import org.junit.jupiter.api.Assertions;
//import org.junit.jupiter.api.Test;
//import org.mockito.InjectMocks;
//import org.mockito.Mock;
//import org.springframework.boot.test.context.SpringBootTest;
//import org.springframework.http.HttpStatus;
//import org.springframework.http.ResponseEntity;
//import org.springframework.security.crypto.password.PasswordEncoder;
//import ua.marketplace.data.Error;
//import ua.marketplace.dto.CodeDto;
//import ua.marketplace.dto.PhoneNumberDto;
//import ua.marketplace.entities.User;
//import ua.marketplace.entities.VerificationCode;
//import ua.marketplace.repositoryes.UserRepository;
//import ua.marketplace.repositoryes.VerificationCodeRepository;
//import ua.marketplace.requests.PhoneCodeRequest;
//import ua.marketplace.requests.PhoneNumberRequest;
//import ua.marketplace.requests.RegistrationRequest;
//import ua.marketplace.responses.CustomResponse;
//import ua.marketplace.security.JwtUtil;
//
//import java.time.LocalDateTime;
//import java.util.Collections;
//import java.util.Objects;
//import java.util.Optional;
//
//import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
//import static org.mockito.ArgumentMatchers.any;
//import static org.mockito.Mockito.when;
//
///**
// * Unit tests for the AuthService class.
// */
//@SpringBootTest
//class PhoneNumberServiceTest {
//
//    @Mock
//    private UserRepository userRepository;
//    @Mock
//    private PasswordEncoder passwordEncoder;
//    @Mock
//    private VerificationCodeRepository verificationCodeRepository;
//    @Mock
//    private JwtUtil jwtUtil;
//    @InjectMocks
//    private PhoneNumberRegistrationService regService;
//
//    /**
//     * Test for successful user registration.
//     */
//    @Test
//    void testRegistrationSuccessfully() {
//
//        //Given
//        RegistrationRequest request = RegistrationRequest
//                .builder()
//                .phoneNumber("111111111")
//                .firstName("Test")
//                .build();
//
//        User user = User
//                .builder()
//                .phoneNumber(request.getPhoneNumber())
//                .firstName(request.getFirstName())
//                .build();
//
//        when(userRepository.existsByPhoneNumber(request.getPhoneNumber())).thenReturn(false);
//        when(userRepository.save(user)).thenReturn(user);
//
//        PhoneNumberDto phoneNumberDto = PhoneNumberDto.builder().phoneNumber(user.getPhoneNumber()).build();
//        ResponseEntity<CustomResponse<PhoneNumberDto>> expect = ResponseEntity.ok
//                (CustomResponse.successfully(phoneNumberDto, HttpStatus.OK.value()));
//
//        //When
//        ResponseEntity<CustomResponse<PhoneNumberDto>> result = regService.registrationUser(request);
//
//        //Then
//        assertThat(result).isEqualTo(expect);
//    }
//
//    /**
//     * Test for user registration with an existing phone number.
//     */
//    @Test
//    void testRegistrationWithPhoneExist() {
//
//        //Given
//        when(userRepository.existsByPhoneNumber(any())).thenReturn(true);
//
//        ResponseEntity<CustomResponse<PhoneNumberDto>> expect = ResponseEntity
//                .badRequest()
//                .body(CustomResponse.failed
//                        (Collections.singletonList(Error.PHONE_ALREADY_EXIST.getMessage()),
//                                HttpStatus.BAD_REQUEST.value()));
//
//        //When
//        ResponseEntity<CustomResponse<PhoneNumberDto>> result = regService.registrationUser(new RegistrationRequest());
//
//        //Then
//        assertThat(result).isEqualTo(expect);
//    }
//
//    /**
//     * Test for successful user login.
//     */
//    @Test
//    void testLoginCodeSuccessfully() {
//
//        //Given
//        PhoneCodeRequest request = PhoneCodeRequest
//                .builder()
//                .phoneNumber("+380123456789")
//                .inputCode("1111")
//                .build();
//
//        User user = User
//                .builder()
//                .phoneNumber(request.getPhoneNumber())
//                .build();
//
//        VerificationCode verificationCode = VerificationCode.builder()
//                .code("1111")
//                .createdTimeCode(LocalDateTime.now())
//                .isEntryByCode(true)
//                .loginAttempt(0)
//                .build();
//
//        user.setVerificationCode(verificationCode);
//
//        CodeDto authDto = CodeDto
//                .builder()
//                .token("test")
//                .build();
//
//        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.of(user));
//        when(passwordEncoder.matches(any(), any())).thenReturn(true);
//
//        //When
//        ResponseEntity<CustomResponse<CodeDto>> result = regService.inputPhoneCode(request);
//        authDto.setToken(Objects.requireNonNull(result.getBody()).getBody().getToken());
//
//        //Then
//        assertThat(result.getStatusCode()).isEqualTo(HttpStatus.OK);
//        assertThat(result.getBody()).isNotNull();
//        assertThat(result.getBody().getBody()).isEqualTo(authDto);
//    }
//
//    /**
//     * Test for user login with user not found.
//     */
//    @Test
//    void testLoginCodeWithUserNotFound() {
//
//        //Given
//        PhoneCodeRequest request = PhoneCodeRequest
//                .builder()
//                .phoneNumber("+123456789012")
//                .inputCode("1234")
//                .build();
//
//        when(userRepository.findByPhoneNumber(any())).thenReturn(Optional.empty());
//
//        // When
//        ResponseEntity<CustomResponse<CodeDto>> result = regService.inputPhoneCode(request);
//
//        // Then
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
//        Assertions.assertTrue(Objects.requireNonNull(result.getBody()).getMessage().contains(Error.USER_NOT_FOUND.getMessage()));
//    }
//
//    /**
//     * Test for user login with an invalid password.
//     */
//    @Test
//    void testLoginWithInvalidCode() {
//
//        //Given
//        PhoneCodeRequest request = PhoneCodeRequest
//                .builder()
//                .phoneNumber("+123456789012")
//                .inputCode("1234")
//                .build();
//
//        User user = User
//                .builder()
//                .phoneNumber(request.getPhoneNumber())
//                .build();
//
//        VerificationCode verificationCode = VerificationCode.builder()
//                .code("1111")
//                .createdTimeCode(LocalDateTime.now())
//                .isEntryByCode(true)
//                .loginAttempt(0)
//                .build();
//
//        user.setVerificationCode(verificationCode);
//
//        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.of(user));
//
//        // When
//        ResponseEntity<CustomResponse<CodeDto>> result = regService.inputPhoneCode(request);
//
//        // Then
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
//        Assertions.assertTrue(Objects.requireNonNull(result
//                        .getBody())
//                .getMessage()
//                .contains(Error.INVALID_CODE.getMessage()));
//    }
//
//    @Test
//    void testLoginCodeWithAccessFalse() {
//
//        //Given
//        PhoneCodeRequest request = PhoneCodeRequest
//                .builder()
//                .phoneNumber("+123456789012")
//                .inputCode("1234")
//                .build();
//
//        User user = User
//                .builder()
//                .phoneNumber(request.getPhoneNumber())
//                .build();
//
//        VerificationCode verificationCode = VerificationCode.builder()
//                .code("1234")
//                .createdTimeCode(LocalDateTime.now())
//                .isEntryByCode(false)
//                .loginAttempt(0)
//                .build();
//
//        user.setVerificationCode(verificationCode);
//
//        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.of(user));
//
//        // When
//        ResponseEntity<CustomResponse<CodeDto>> result = regService.inputPhoneCode(request);
//
//        // Then
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
//        Assertions.assertTrue(Objects.requireNonNull(result
//                        .getBody())
//                .getMessage()
//                .contains(Error.ACCESS_FALSE.getMessage()));
//    }
//
//    @Test
//    void testLoginCodeWithTimeIsUp() {
//
//        //Given
//        PhoneCodeRequest request = PhoneCodeRequest
//                .builder()
//                .phoneNumber("+123456789012")
//                .inputCode("1234")
//                .build();
//
//        User user = User
//                .builder()
//                .phoneNumber(request.getPhoneNumber())
//                .build();
//
//        VerificationCode verificationCode = VerificationCode.builder()
//                .code("1234")
//                .createdTimeCode(LocalDateTime.now().minusMinutes(10))
//                .isEntryByCode(true)
//                .loginAttempt(0)
//                .build();
//
//        user.setVerificationCode(verificationCode);
//        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.of(user));
//
//        // When
//        ResponseEntity<CustomResponse<CodeDto>> result = regService.inputPhoneCode(request);
//
//        // Then
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
//        Assertions.assertTrue(Objects.requireNonNull(result
//                        .getBody())
//                .getMessage()
//                .contains(Error.TIME_IS_UP.getMessage()));
//    }
//
//    @Test
//    void testLoginCodeWithMaxInputCode() {
//
//        //Given
//        PhoneCodeRequest request = PhoneCodeRequest
//                .builder()
//                .phoneNumber("+123456789012")
//                .inputCode("1234")
//                .build();
//
//        User user = User
//                .builder()
//                .phoneNumber(request.getPhoneNumber())
//                .build();
//
//        VerificationCode verificationCode = VerificationCode.builder()
//                .code("1111")
//                .createdTimeCode(LocalDateTime.now())
//                .isEntryByCode(true)
//                .loginAttempt(3)
//                .build();
//
//        user.setVerificationCode(verificationCode);
//        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.of(user));
//
//        // When
//        ResponseEntity<CustomResponse<CodeDto>> result = regService.inputPhoneCode(request);
//
//        // Then
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
//        Assertions.assertTrue(Objects.requireNonNull(result
//                        .getBody())
//                .getMessage()
//                .contains(Error.MAX_INPUT_CODE.getMessage()));
//    }
//
//    @Test
//    void testLoginSuccessfully() {
//
//        //Given
//        PhoneNumberRequest request = PhoneNumberRequest
//                .builder()
//                .phoneNumber("+380987654321")
//                .build();
//
//        User user = User
//                .builder()
//                .phoneNumber(request.getPhoneNumber())
//                .build();
//
//        VerificationCode verificationCode = VerificationCode.builder()
//                .code("1111")
//                .createdTimeCode(LocalDateTime.now().minusMinutes(2))
//                .isEntryByCode(true)
//                .loginAttempt(0)
//                .build();
//
//        user.setVerificationCode(verificationCode);
//        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.of(user));
//
//        PhoneNumberDto phoneNumberDto = PhoneNumberDto.builder().phoneNumber(user.getPhoneNumber()).build();
//        ResponseEntity<CustomResponse<PhoneNumberDto>> expect = ResponseEntity.ok
//                (CustomResponse.successfully(phoneNumberDto, HttpStatus.OK.value()));
//
//        //When
//        ResponseEntity<CustomResponse<PhoneNumberDto>> result = regService.inputPhoneNumber(request);
//
//        //Then
//        assertThat(result).isEqualTo(expect);
//    }
//
//    @Test
//    void testLoginFailedWithUserNotFound() {
//
//        //Given
//        PhoneNumberRequest request = PhoneNumberRequest
//                .builder()
//                .phoneNumber("+380987654321")
//                .build();
//
//        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.empty());
//
//        // When
//        ResponseEntity<CustomResponse<PhoneNumberDto>> result = regService.inputPhoneNumber(request);
//
//        // Then
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
//        Assertions.assertTrue(Objects.requireNonNull(result
//                        .getBody())
//                .getMessage()
//                .contains(Error.USER_NOT_FOUND.getMessage()));
//    }
//
//    @Test
//    void testLoginFailedWithTimeAccess() {
//
//        //Given
//        PhoneNumberRequest request = PhoneNumberRequest
//                .builder()
//                .phoneNumber("+380987654321")
//                .build();
//
//        User user = User
//                .builder()
//                .phoneNumber("+380987654321")
//                .build();
//
//        VerificationCode verificationCode = VerificationCode.builder()
//                .code("1111")
//                .createdTimeCode(LocalDateTime.now())
//                .isEntryByCode(true)
//                .loginAttempt(0)
//                .build();
//
//        user.setVerificationCode(verificationCode);
//        when(userRepository.findByPhoneNumber(request.getPhoneNumber())).thenReturn(Optional.of(user));
//
//        // When
//        ResponseEntity<CustomResponse<PhoneNumberDto>> result = regService.inputPhoneNumber(request);
//
//        // Then
//        Assertions.assertEquals(HttpStatus.BAD_REQUEST, result.getStatusCode());
//        Assertions.assertTrue(Objects.requireNonNull(result
//                        .getBody())
//                .getMessage()
//                .contains(Error.ACCESS_STOP_FOR_1_MINUTE.getMessage()));
//    }
//}