package ua.marketplace.services;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import ua.marketplace.data.Error;
import ua.marketplace.dto.CodeDto;
import ua.marketplace.dto.PhoneNumberDto;
import ua.marketplace.entities.User;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.PhoneCodeRequest;
import ua.marketplace.requests.PhoneNumberRequest;
import ua.marketplace.responses.CustomResponse;
import ua.marketplace.security.JwtUtil;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Optional;
import java.util.UUID;

/**
 * Service class responsible for handling phone number registration operations.
 */
@Service
@RequiredArgsConstructor
public class PhoneNumberRegistrationService implements IPhoneNumberRegistrationService {

    private final UserRepository userRepository;
    private final JwtUtil jwtUtil;

    /**
     * Handles the registration of a new phone number.
     *
     * @param request PhoneNumberRequest containing the phone number to be registered.
     * @return ResponseEntity with CustomResponse containing the registered phone number or error message.
     */
    @Override
    public ResponseEntity<CustomResponse<PhoneNumberDto>> inputPhoneNumber(PhoneNumberRequest request) {
        if (Boolean.TRUE.equals(userRepository.existsByPhone(request.getPhoneNumber()))) {
            CustomResponse<PhoneNumberDto> response = CustomResponse.failed(
                    Collections.singletonList(Error.PHONE_ALREADY_EXIST.getMessage()),
                    HttpStatus.BAD_REQUEST.value());
            return ResponseEntity.badRequest().body(response);
        }

        User createdUser = User.builder()
                .phone(request.getPhoneNumber())
                .password(UUID.randomUUID().toString().substring(0, 10))
                .code("999999")
                .role("USER")
                .isEnabled(true)
                .createdTimeCode(LocalDateTime.now())
                .build();
        userRepository.save(createdUser);

        PhoneNumberDto phoneNumberDto = PhoneNumberDto.builder().phoneNumber(createdUser.getPhone()).build();
        CustomResponse<PhoneNumberDto> response = CustomResponse.successfully(phoneNumberDto, HttpStatus.OK.value());
        return ResponseEntity.ok(response);
    }

    /**
     * Handles the input of a phone code during registration.
     *
     * @param request PhoneCodeRequest containing the phone number and input code.
     * @return ResponseEntity with CustomResponse containing the JWT token or error message.
     */
    @Override
    public ResponseEntity<CustomResponse<CodeDto>> inputPhoneCode(PhoneCodeRequest request) {
        Optional<User> byPhone = userRepository.findByPhone(request.getPhoneNumber());
        if (byPhone.isEmpty()) {
            CustomResponse<CodeDto> response = CustomResponse.failed(
                    Collections.singletonList(Error.USER_NOT_FOUND.getMessage()),
                    HttpStatus.BAD_REQUEST.value());
            return ResponseEntity.badRequest().body(response);
        }

        User user = byPhone.get();
        if (!user.getCode().equals(request.getInputCode())) {
            CustomResponse<CodeDto> response = CustomResponse.failed(
                    Collections.singletonList(Error.INVALID_CODE.getMessage()),
                    HttpStatus.BAD_REQUEST.value());
            return ResponseEntity.badRequest().body(response);
        }

        LocalDateTime userTimeAccess = user.getCreatedTimeCode().plusMinutes(5);
        if (userTimeAccess.isBefore(LocalDateTime.now())) {
            CustomResponse<CodeDto> response = CustomResponse.failed(
                    Collections.singletonList(Error.TIME_IS_UP.getMessage()),
                    HttpStatus.BAD_REQUEST.value());
            return ResponseEntity.badRequest().body(response);
        }

        CodeDto codeDto = CodeDto.builder()
                .token(jwtUtil.generateToken(user.getPhone()))
                .build();
        CustomResponse<CodeDto> response = CustomResponse.successfully(codeDto, HttpStatus.OK.value());
        return ResponseEntity.ok(response);
    }
}