package ua.marketplace.services;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.password.PasswordEncoder;
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

    public ResponseEntity<CustomResponse<UserDto>> registration(RegistrationRequest request) {

        if (Boolean.TRUE.equals(userRepository.existsByPhoneNumber(request.getPhoneNumber()))) {
            return ResponseEntity
                    .badRequest()
                    .body(CustomResponse.failed(Collections.singletonList(Error.PHONE_ALREADY_EXIST.getMessage()),
                            HttpStatus.BAD_REQUEST.value()));
        }

        User user = createUser(request);
        userRepository.save(user);
        return ResponseEntity
                .ok()
                .body(CustomResponse.successfully(userService.convertToDto(user),
                        HttpStatus.OK.value()));
    }

    public ResponseEntity<CustomResponse<UserDto>> login(LoginRequest request) {

        Optional<User> byPhoneNumber = userRepository.findByPhoneNumber(request.getPhoneNumber());

        if (byPhoneNumber.isEmpty()) {
            return ResponseEntity
                    .badRequest()
                    .body(CustomResponse.failed(Collections.singletonList(Error.USER_NOT_FOUND.getMessage()),
                            HttpStatus.BAD_REQUEST.value()));
        }

        User user = byPhoneNumber.get();
        user.setCode(codeService.sendCode(request.getPhoneNumber()));
        userRepository.save(user);

        return ResponseEntity
                .ok()
                .body(CustomResponse.successfully(userService.convertToDto(user),
                        HttpStatus.OK.value()));
    }

    public ResponseEntity<CustomResponse<AuthDto>> checkCode(CheckCodeRequest request) {

        Optional<User> byCode = userRepository.findByCode(request.getCode());

        if (byCode.isEmpty()) {
            return ResponseEntity
                    .badRequest()
                    .body(CustomResponse.failed(Collections.singletonList(Error.USER_NOT_FOUND.getMessage()),
                            HttpStatus.BAD_REQUEST.value()));
        }

        User user = byCode.get();
        user.setCode(null);
        userRepository.save(user);
        String token = jwtUtil.generateToken(user.getPhoneNumber());

        AuthDto authDto = AuthDto
                .builder()
                .user(userService.convertToDto(user))
                .token(token)
                .build();

        return ResponseEntity
                .ok()
                .body(CustomResponse.successfully(authDto,
                        HttpStatus.OK.value()));
    }

    private User createUser(RegistrationRequest request) {
        return User
                .builder()
                .name(request.getName())
                .phoneNumber(request.getPhoneNumber())
                .build();

    }
}