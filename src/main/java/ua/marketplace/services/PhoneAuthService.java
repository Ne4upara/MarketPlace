package ua.marketplace.services;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import ua.marketplace.data.Error;
import ua.marketplace.dto.AuthDto;
import ua.marketplace.dto.UserDto;
import ua.marketplace.entities.SmsCode;
import ua.marketplace.entities.User;
import ua.marketplace.repositoryes.SmsCodeRepository;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.CheckCodeRequest;
import ua.marketplace.requests.LoginRequest;
import ua.marketplace.requests.RegistrationRequest;
import ua.marketplace.responses.CustomResponse;
import ua.marketplace.security.JwtUtil;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Optional;

/**
 * Service class responsible for handling phoneNumber number registerUser operations.
 */
@Service
@RequiredArgsConstructor
public class PhoneAuthService {

    private final UserRepository userRepository;
    private final SmsCodeRepository smsCodeRepository;
    private final UserService userService;
    private final CodeService codeService;
    private final JwtUtil jwtUtil;

    /**
     * Registers a new user with the provided registerUser request.
     *
     * @param request RegistrationRequest object containing user's registerUser data.
     * @return ResponseEntity containing CustomResponse with UserDto if registerUser is successful,
     * or a bad request response with error message if phone number already exists.
     */
    public ResponseEntity<CustomResponse<UserDto>> registerUser(RegistrationRequest request) {

        if (Boolean.TRUE.equals(userRepository.existsByPhoneNumber(request.getPhoneNumber()))) {
            return authBadRequest(Error.PHONE_ALREADY_EXIST);
        }

        User user = createUser(request);
        userRepository.save(user);

        return authOk(user);
    }

    /**
     * Logs in a user with the provided loginUser request.
     *
     * @param request LoginRequest object containing user's loginUser data.
     * @return ResponseEntity containing CustomResponse with UserDto if loginUser is successful,
     * or a bad request response with error message if user is not found.
     */
    public ResponseEntity<CustomResponse<UserDto>> loginUser(LoginRequest request) {

        Optional<User> byPhoneNumber = userRepository.findByPhoneNumber(request.getPhoneNumber());

        if (byPhoneNumber.isEmpty()) {
            return authBadRequest(Error.USER_NOT_FOUND);
        }

        User user = byPhoneNumber.get();
        updateUserSmsCode(user, codeService.sendCode(request.getPhoneNumber()));

        return authOk(user);
    }

    /**
     * Checks the verification code for a user's loginUser.
     *
     * @param request CheckCodeRequest object containing the code to be verified.
     * @return ResponseEntity containing CustomResponse with AuthDto if code verification is successful,
     * or a bad request response with error message if user is not found.
     */
    public ResponseEntity<CustomResponse<AuthDto>> checkVerificationCode(CheckCodeRequest request) {

        Optional<User> byPhoneNumber = userRepository.findByPhoneNumber(request.getPhoneNumber());

        if (byPhoneNumber.isEmpty()) {
            return checkCodeBadRequest(Error.USER_NOT_FOUND);
        }

        User user = byPhoneNumber.get();

        if (!user.getSmsCode().isEnable()) {
            return checkCodeBadRequest(Error.CODE_NOT_ENABLE);
        }

        if (isCodeTimeOut(user)) {
            return checkCodeBadRequest(Error.CODE_TIME_IS_OUT);
        }

        int attempts = user.getSmsCode().getVerificationAttempts();

        if (attempts <= 0) {
            return checkCodeBadRequest(Error.TRY_IS_OUT);
        }

        if (!request.getSmsCode().equals(user.getSmsCode().getCode())) {
            user.getSmsCode().setVerificationAttempts(attempts - 1);
            userRepository.save(user);
            return checkCodeBadRequest(Error.INVALID_CODE);
        }

        AuthDto authDto = createAuthDto(user);
        disableSmsCode(user);

        return checkCodeOk(authDto);
    }

    /**
     * Constructs a bad request response entity with the provided error message.
     *
     * @param error Error enum indicating the type of error.
     * @return Bad request response entity with error message.
     */
    private ResponseEntity<CustomResponse<UserDto>> authBadRequest(Error error) {
        return ResponseEntity
                .badRequest()
                .body(CustomResponse.failed(Collections.singletonList(error.getMessage()),
                        HttpStatus.BAD_REQUEST.value()));
    }

    /**
     * Constructs an ok response entity with the provided user information.
     *
     * @param user User object containing user information.
     * @return Ok response entity with user information.
     */
    private ResponseEntity<CustomResponse<UserDto>> authOk(User user) {
        return ResponseEntity
                .ok()
                .body(CustomResponse.successfully(userService.convertToDto(user),
                        HttpStatus.OK.value()));
    }

    /**
     * Constructs a bad request response entity indicating code verification failure.
     *
     * @return Bad request response entity indicating code verification failure.
     */
    private ResponseEntity<CustomResponse<AuthDto>> checkCodeBadRequest(Error error) {
        return ResponseEntity
                .badRequest()
                .body(CustomResponse.failed(Collections.singletonList(error.getMessage()),
                        HttpStatus.BAD_REQUEST.value()));
    }

    /**
     * Constructs an ok response entity with the provided authentication information.
     *
     * @param authDto AuthDto object containing authentication information.
     * @return Ok response entity with authentication information.
     */
    private ResponseEntity<CustomResponse<AuthDto>> checkCodeOk(AuthDto authDto) {
        return ResponseEntity
                .ok()
                .body(CustomResponse.successfully(authDto,
                        HttpStatus.OK.value()));
    }

    /**
     * Creates a new User entity based on the provided registerUser request.
     *
     * @param request RegistrationRequest object containing user's registerUser data.
     * @return Newly created User entity.
     */
    private User createUser(RegistrationRequest request) {
        return User
                .builder()
                .firstName(request.getFirstName())
                .phoneNumber(request.getPhoneNumber())
                .build();

    }

    /**
     * Creates an authentication DTO with the provided user and token information.
     *
     * @param user User object containing user information.
     * @return AuthDto object containing authentication information.
     */
    private AuthDto createAuthDto(User user) {
        return AuthDto
                .builder()
                .user(userService.convertToDto(user))
                .token(jwtUtil.generateToken(user.getPhoneNumber()))
                .build();

    }

    /**
     * Disable the SMS code for the user
     *
     * @param user User object whose SMS code information needs to be disabled.
     */
    private void disableSmsCode(User user) {
        user.getSmsCode().setEnable(false);
        userRepository.save(user);
    }

    /**
     * Checks if the verification code for the user has timed out.
     *
     * @param user User object for whom the verification code is being checked.
     * @return True if the code has timed out, otherwise false.
     */
    private boolean isCodeTimeOut(User user) {
        LocalDateTime expirationTime = user.getSmsCode().getCreateAt().plusMinutes(1);
        return LocalDateTime.now().isAfter(expirationTime);
    }

    /**
     * Update SMS code information for user.
     *
     * @param user User object for whom updated SMS code information.
     * @param code SMS code for updating information.
     */
    private void updateUserSmsCode(User user, String code) {

        if (user.getSmsCode() == null) {
            SmsCode smsCode = SmsCode
                    .builder()
                    .code(code)
                    .createAt(LocalDateTime.now())
                    .isEnable(true)
                    .verificationAttempts(3)
                    .build();

            user.setSmsCode(smsCode);
            userRepository.save(user);

        } else {
            SmsCode smsCode = user.getSmsCode();
            smsCode.setCode(code);
            smsCode.setCreateAt(LocalDateTime.now());
            smsCode.setEnable(true);
            smsCode.setVerificationAttempts(3);

            smsCodeRepository.save(smsCode);
        }
    }
}
