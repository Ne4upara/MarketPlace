package ua.marketplace.services;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
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
        user.setSmsCode(codeService.sendCode(request.getPhoneNumber()));
        user.setSmsCodeCreateAt(LocalDateTime.now());
        userRepository.save(user);

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

        if (isCodeTimeOut(user)) {
            return checkCodeBadRequest(Error.CODE_TIME_IS_OUT);
        }

        if (!request.getSmsCode().equals(user.getSmsCode())) {
            return checkCodeBadRequest(Error.INVALID_CODE);
        }

        AuthDto authDto = createAuthDto(user);
        updateUserSmsCodeInfo(user);

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
     * Updates the SMS code information for the user.
     *
     * @param user User object whose SMS code information needs to be updated.
     */
    private void updateUserSmsCodeInfo(User user) {
        user.setSmsCode(null);
        userRepository.save(user);
    }

    /**
     * Checks if the verification code for the user has timed out.
     *
     * @param user User object for whom the verification code is being checked.
     * @return True if the code has timed out, otherwise false.
     */
    private boolean isCodeTimeOut(User user) {
        LocalDateTime expirationTime = user.getSmsCodeCreateAt().plusMinutes(1); //
        return LocalDateTime.now().isAfter(expirationTime);
    }
}
