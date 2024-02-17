package ua.marketplace.services;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import ua.marketplace.data.Error;
import ua.marketplace.dto.CodeDto;
import ua.marketplace.dto.PhoneNumberDto;
import ua.marketplace.entities.User;
import ua.marketplace.entities.VerificationCode;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.repositoryes.VerificationCodeRepository;
import ua.marketplace.requests.PhoneCodeRequest;
import ua.marketplace.requests.PhoneNumberRequest;
import ua.marketplace.requests.RegistrationRequest;
import ua.marketplace.responses.CustomResponse;
import ua.marketplace.security.JwtUtil;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Optional;

/**
 * The class of service responsible for processing phone number registration and login transactions.
 */
@Service
@RequiredArgsConstructor
public class PhoneNumberRegistrationService implements IPhoneNumberRegistrationService {

    private final UserRepository userRepository;
    private final VerificationCodeRepository verificationCodeRepository;
    private final JwtUtil jwtUtil;

    /**
     * Processes a request to enter a phone number to log in to the system.
     *
     * @param request PhoneNumberRequest containing the phone number to be log in to the system.
     * @return ResponseEntity containing CustomResponse with UserDto if registerUser is successful,
     *      * or a bad request response with error message user not found, access stop for 1 minute.
     */
    @Override
    public ResponseEntity<CustomResponse<PhoneNumberDto>> inputPhoneNumber(PhoneNumberRequest request) {
        Optional<User> byPhoneNumber = userRepository.findByPhoneNumber(request.getPhoneNumber());
        if(byPhoneNumber.isEmpty()){
            return ResponseEntity.badRequest().body(getErrorMessage(PhoneNumberDto.class, Error.USER_NOT_FOUND));
        }

        User user = byPhoneNumber.get();
        if(isTimeUp(user.getVerificationCode(), 1, false)){
            return ResponseEntity.badRequest()
                    .body(getErrorMessage(PhoneNumberDto.class, Error.ACCESS_STOP_FOR_1_MINUTE));
        }

        user.setVerificationCode(updateVerificationCode(user));
        userRepository.save(user);
        PhoneNumberDto phoneNumberDto = PhoneNumberDto.builder().phoneNumber(request.getPhoneNumber()).build();
        CustomResponse<PhoneNumberDto> response = CustomResponse.successfully(phoneNumberDto, HttpStatus.OK.value());
        return ResponseEntity.ok(response);
    }

    /**
     * Checks the verification code for a user's loginUser.
     *
     * @param request PhoneCodeRequest object containing the code to be verified and phone number.
     * @return ResponseEntity containing CustomResponse with CodeDto if code verification is successful,
     * or a bad request response with error message if user is not found, access false, max input code, invalid code
     * time is up.
     */
    @Override
    public ResponseEntity<CustomResponse<CodeDto>> inputPhoneCode(PhoneCodeRequest request) {
        Optional<User> byPhone = userRepository.findByPhoneNumber(request.getPhoneNumber());
        if (byPhone.isEmpty()) {
            return ResponseEntity.badRequest().body(getErrorMessage(CodeDto.class, Error.USER_NOT_FOUND));
        }

        User user = byPhone.get();
        VerificationCode verificationCode = user.getVerificationCode();
        if(Boolean.FALSE.equals(verificationCode.getIsEntryByCode())){
            return ResponseEntity.badRequest().body(getErrorMessage(CodeDto.class, Error.ACCESS_FALSE));
        }

        if (!verificationCode.getCode().equals(request.getInputCode())){
            if(verificationCode.getLoginAttempt() == 3){
                return ResponseEntity.badRequest().body(getErrorMessage(CodeDto.class, Error.MAX_INPUT_CODE));
            }

            verificationCode.setLoginAttempt(verificationCode.getLoginAttempt() + 1);
            verificationCodeRepository.save(verificationCode);
            return ResponseEntity.badRequest().body(getErrorMessage(CodeDto.class, Error.INVALID_CODE));
        }

        if (isTimeUp(verificationCode, 5, true)) {
            return ResponseEntity.badRequest().body(getErrorMessage(CodeDto.class, Error.TIME_IS_UP));
        }

        CodeDto codeDto = CodeDto.builder()
                .token(jwtUtil.generateToken(user.getPhoneNumber()))
                .firstName(user.getFirstName())
                .build();
        CustomResponse<CodeDto> response = CustomResponse.successfully(codeDto, HttpStatus.OK.value());
        verificationCode.setIsEntryByCode(false);
        verificationCodeRepository.save(verificationCode);
        return ResponseEntity.ok(response);
    }

    /**
     * Registers a new user with the provided registerUser request.
     *
     * @param request RegistrationRequest object containing user's registerUser data.
     * @return ResponseEntity containing CustomResponse with PhoneNumberDto if registerUser is successful,
     * or a bad request response with error message if phone number already exists.
     */
    @Override
    public ResponseEntity<CustomResponse<PhoneNumberDto>> registrationUser(RegistrationRequest request) {
        if (Boolean.TRUE.equals(userRepository.existsByPhoneNumber(request.getPhoneNumber()))) {
            return ResponseEntity.badRequest().body(getErrorMessage(PhoneNumberDto.class, Error.PHONE_ALREADY_EXIST));
        }

        User user = User.builder()
                .firstName(request.getFirstName())
                .phoneNumber(request.getPhoneNumber())
                .isEnabled(true)
                .role("USER")
                .build();
        user.setVerificationCode(createdVerificationCode(user));
        userRepository.save(user);
        PhoneNumberDto phoneNumberDto = PhoneNumberDto.builder().phoneNumber(request.getPhoneNumber()).build();
        CustomResponse<PhoneNumberDto> response = CustomResponse.successfully(phoneNumberDto, HttpStatus.OK.value());
        return ResponseEntity.ok(response);
    }

    /**
     * Generates a custom error response based on the provided error message and HTTP status code.
     *
     * @param ignoredDtoClass The ignored class type (not used in the method logic).
     * @param error The error enumeration representing the error message.
     * @param <T> The type of the DTO in the custom response.
     * @return A custom response containing the error message and HTTP status code.
     */
    private <T> CustomResponse<T> getErrorMessage(Class<?> ignoredDtoClass, Error error) {
        return CustomResponse.failed(
                Collections.singletonList(error.getMessage()),
                HttpStatus.BAD_REQUEST.value());
    }

    private VerificationCode updateVerificationCode(User user){
        VerificationCode verificationCode = user.getVerificationCode();
        verificationCode.setCode("2222");  //Вставить метод генерации кода
        verificationCode.setCreatedTimeCode(LocalDateTime.now());
        verificationCode.setLoginAttempt(0);
        verificationCode.setIsEntryByCode(true);
        return verificationCode;
    }

    private VerificationCode createdVerificationCode(User user){
        return VerificationCode.builder()
                .code("1111")   //Вставить метод генерации кода
                .createdTimeCode(LocalDateTime.now())
                .isEntryByCode(true)
                .loginAttempt(0)
                .user(user)
                .build();
    }

    private boolean isTimeUp(VerificationCode verificationCode, int minutes, boolean isBefore){
        LocalDateTime userTimeAccess = verificationCode.getCreatedTimeCode().plusMinutes(minutes);
        if (isBefore) {
            return userTimeAccess.isBefore(LocalDateTime.now());
        } else {
            return userTimeAccess.isAfter(LocalDateTime.now());
        }
    }
}