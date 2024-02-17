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
 * Service class responsible for handling phone number registration operations.
 */
@Service
@RequiredArgsConstructor
public class PhoneNumberRegistrationService implements IPhoneNumberRegistrationService {

    private final UserRepository userRepository;
    private final VerificationCodeRepository verificationCodeRepository;
    private final JwtUtil jwtUtil;

    /**
     * Handles the registration of a new phone number.
     *
     * @param request PhoneNumberRequest containing the phone number to be registered.
     * @return ResponseEntity with CustomResponse containing the registered phone number or error message.
     */
    @Override
    public ResponseEntity<CustomResponse<PhoneNumberDto>> inputPhoneNumber(PhoneNumberRequest request) {
        Optional<User> byPhoneNumber = userRepository.findByPhoneNumber(request.getPhoneNumber());
        if(byPhoneNumber.isEmpty()){
            return ResponseEntity.badRequest().body(getErrorMessage(PhoneNumberDto.class, Error.USER_NOT_FOUND));
        }

        User user = byPhoneNumber.get();
        updateVerificationCode(user);
        PhoneNumberDto phoneNumberDto = PhoneNumberDto.builder().phoneNumber(request.getPhoneNumber()).build();
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

        LocalDateTime userTimeAccess = verificationCode.getCreatedTimeCode().plusMinutes(5);
        if (userTimeAccess.isBefore(LocalDateTime.now())) {
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
     *
     * @param request
     * @return
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
        createdVerificationCode(user);
        PhoneNumberDto phoneNumberDto = PhoneNumberDto.builder().phoneNumber(request.getPhoneNumber()).build();
        CustomResponse<PhoneNumberDto> response = CustomResponse.successfully(phoneNumberDto, HttpStatus.OK.value());
        return ResponseEntity.ok(response);
    }

    /**
     *
     * @param ignoredDtoClass - enter class
     * @param error
     * @param <T>
     */

    private <T> CustomResponse<T> getErrorMessage(Class<T> ignoredDtoClass, Error error) {
        return CustomResponse.failed(
                Collections.singletonList(error.getMessage()),
                HttpStatus.BAD_REQUEST.value());
    }

    private void updateVerificationCode(User user){
        VerificationCode verificationCode = user.getVerificationCode();
        verificationCode.setCode("2222");//метод генерации кода
        verificationCode.setCreatedTimeCode(LocalDateTime.now());
        verificationCode.setLoginAttempt(0);
        verificationCode.setIsEntryByCode(true);
        user.setVerificationCode(verificationCode);
        userRepository.save(user);
    }

    private void createdVerificationCode(User user){
        VerificationCode verificationCode = VerificationCode.builder()
                .code("1111") //Вставить метод генерации кода
                .createdTimeCode(LocalDateTime.now())
                .isEntryByCode(true)
                .loginAttempt(0)
                .user(user)
                .build();
        user.setVerificationCode(verificationCode);
        userRepository.save(user);
    }

}