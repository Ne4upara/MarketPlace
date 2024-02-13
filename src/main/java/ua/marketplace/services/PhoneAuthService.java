package ua.marketplace.services;

import jakarta.transaction.Transactional;
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

import java.util.Collections;
import java.util.Optional;

/**
 * Service class responsible for handling phoneNumber number registration operations.
 */
@Service
@RequiredArgsConstructor
public class PhoneAuthService {

    private final UserRepository userRepository;
    private final UserService userService;
    private final CodeService codeService;
    private final JwtUtil jwtUtil;

    /**
     * Registers a new user with the provided registration request.
     *
     * @param request RegistrationRequest object containing user's registration data.
     * @return ResponseEntity containing CustomResponse with UserDto if registration is successful,
     *         or a bad request response with error message if phone number already exists.
     */
    @Transactional
    public ResponseEntity<CustomResponse<UserDto>> registration(RegistrationRequest request) {

        if (Boolean.TRUE.equals(userRepository.existsByPhoneNumber(request.getPhoneNumber()))) {
            return authBadRequest(Error.PHONE_ALREADY_EXIST);
        }

        User user = createUser(request);
        userRepository.save(user);

        return authOk(user);
    }

    /**
     * Logs in a user with the provided login request.
     *
     * @param request LoginRequest object containing user's login data.
     * @return ResponseEntity containing CustomResponse with UserDto if login is successful,
     *         or a bad request response with error message if user is not found.
     */
    public ResponseEntity<CustomResponse<UserDto>> login(LoginRequest request) {

        Optional<User> byPhoneNumber = userRepository.findByPhoneNumber(request.getPhoneNumber());

        if (byPhoneNumber.isEmpty()) {
            return authBadRequest(Error.USER_NOT_FOUND);
        }

        User user = byPhoneNumber.get();
        user.setCode(codeService.sendCode(request.getPhoneNumber()));
        userRepository.save(user);

        return authOk(user);
    }

    /**
     * Checks the verification code for a user's login.
     *
     * @param request CheckCodeRequest object containing the code to be verified.
     * @return ResponseEntity containing CustomResponse with AuthDto if code verification is successful,
     *         or a bad request response with error message if user is not found.
     */
    public ResponseEntity<CustomResponse<AuthDto>> checkCode(CheckCodeRequest request) {

        Optional<User> byCode = userRepository.findByCode(request.getCode());

        if (byCode.isEmpty()) {
            return checkCodeBadRequest();
        }

        User user = byCode.get();
        user.setCode(null);
        userRepository.save(user);
        String token = jwtUtil.generateToken(user.getPhoneNumber());

        AuthDto authDto = createAuthDto(user,token);

        return checkCodeOk(authDto);
    }

    /**
     * Constructs a bad request response entity with the provided error message.
     *
     * @param error Error enum indicating the type of error.
     * @return Bad request response entity with error message.
     */
    private ResponseEntity<CustomResponse<UserDto>> authBadRequest(Error error){
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
    private ResponseEntity<CustomResponse<UserDto>> authOk(User user){
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
    private ResponseEntity<CustomResponse<AuthDto>> checkCodeBadRequest() {
        return ResponseEntity
                .badRequest()
                .body(CustomResponse.failed(Collections.singletonList(Error.USER_NOT_FOUND.getMessage()),
                        HttpStatus.BAD_REQUEST.value()));
    }

    /**
     * Constructs an ok response entity with the provided authentication information.
     *
     * @param authDto AuthDto object containing authentication information.
     * @return Ok response entity with authentication information.
     */
    private ResponseEntity<CustomResponse<AuthDto>> checkCodeOk(AuthDto authDto){
        return ResponseEntity
                .ok()
                .body(CustomResponse.successfully(authDto,
                        HttpStatus.OK.value()));
    }

    /**
     * Creates a new User entity based on the provided registration request.
     *
     * @param request RegistrationRequest object containing user's registration data.
     * @return Newly created User entity.
     */
    private User createUser(RegistrationRequest request) {
        return User
                .builder()
                .name(request.getName())
                .phoneNumber(request.getPhoneNumber())
                .build();

    }

    /**
     * Creates an authentication DTO with the provided user and token information.
     *
     * @param user  User object containing user information.
     * @param token String containing authentication token.
     * @return AuthDto object containing authentication information.
     */
    private AuthDto createAuthDto(User user, String token) {
        return AuthDto
                .builder()
                .user(userService.convertToDto(user))
                .token(token)
                .build();

    }
}