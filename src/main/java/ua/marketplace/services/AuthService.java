package ua.marketplace.services;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import ua.marketplace.data.Error;
import ua.marketplace.dto.AuthDto;
import ua.marketplace.dto.UserDto;
import ua.marketplace.entitys.User;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.LoginRequest;
import ua.marketplace.requests.RegisterRequest;
import ua.marketplace.responses.CustomResponse;
import ua.marketplace.security.JwtUtil;

import java.util.Collections;
import java.util.Optional;

/**
 * Service class responsible for user authentication operations such as registration and login.
 */

@Service
@RequiredArgsConstructor
public class AuthService {

    private final JwtUtil jwtUtil;
    private final UserRepository repository;
    private final PasswordEncoder passwordEncoder;
    private final UserService service;

    /**
     * Registers a new user with the provided phone number and password.
     *
     * @param request the registration request containing user details
     * @return ResponseEntity with CustomResponse containing the registration result
     */
    public ResponseEntity<CustomResponse<UserDto>> registration(RegisterRequest request) {

        if (Boolean.TRUE.equals(repository.existsByPhone(request.getPhone()))) {
            CustomResponse<UserDto> response = CustomResponse.failed
                    (Collections.singletonList(Error.PHONE_ALREADY_EXIST.getMessage()),
                            HttpStatus.BAD_REQUEST.value());

            return ResponseEntity.badRequest().body(response);
        }

        User createdUser = User
                .builder()
                .phone(request.getPhone())
                .password(passwordEncoder.encode(request.getPassword()))
                .build();
        repository.save(createdUser);

        UserDto userDto = service.convertToDto(createdUser);
        CustomResponse<UserDto> response = CustomResponse.successfully(userDto, HttpStatus.OK.value());

        return ResponseEntity.ok(response);
    }

    /**
     * Logs in a user with the provided phone number and password.
     *
     * @param request the login request containing user credentials
     * @return ResponseEntity with CustomResponse containing the login result
     */
    public ResponseEntity<CustomResponse<AuthDto>> login(LoginRequest request) {

        Optional<User> optionalUser = repository.findByPhone(request.getPhone());

        if (optionalUser.isEmpty()) {
            CustomResponse<AuthDto> response = CustomResponse.failed
                    (Collections.singletonList(Error.USER_NOT_FOUND.getMessage()),
                            HttpStatus.BAD_REQUEST.value());

            return ResponseEntity.badRequest().body(response);
        }

        User userFromDb = optionalUser.get();
        if (!passwordEncoder.matches(request.getPassword(), optionalUser.get().getPassword())) {
            CustomResponse<AuthDto> response = CustomResponse.failed
                    (Collections.singletonList(Error.INVALID_PASSWORD.getMessage()),
                            HttpStatus.BAD_REQUEST.value());

            return ResponseEntity.badRequest().body(response);
        }

        AuthDto authDto = AuthDto
                .builder()
                .user(service.convertToDto(userFromDb))
                .token(jwtUtil.generateToken(userFromDb.getPhone()))
                .build();

        CustomResponse<AuthDto> response = CustomResponse.successfully(authDto, HttpStatus.OK.value());

        return ResponseEntity.ok(response);
    }
}