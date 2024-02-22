package ua.marketplace.services;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import ua.marketplace.entities.User;
import ua.marketplace.entities.VerificationCode;
import ua.marketplace.exception.AppException;
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
public class PhoneNumberRegistrationService {

    private final UserRepository userRepository;
    private final VerificationCodeRepository verificationCodeRepository;
    private final JwtUtil jwtUtil;

//    /**
//     * Processes a request to enter a phone number to log in to the system.
//     *
//     * @param request PhoneNumberRequest containing the phone number to be log in to the system.
//     * @return ResponseEntity containing CustomResponse with UserDto if registerUser is successful,
//     *      * or a bad request response with error message user not found, access stop for 1 minute.
//     */
//    @Override
    public User inputPhoneNumber(PhoneNumberRequest request) throws AppException {
        Optional<User> byPhoneNumber = userRepository.findByPhoneNumber(request.getPhoneNumber());
        if(byPhoneNumber.isEmpty()){
            throw new AppException("User with this phone not found " + request.getPhoneNumber());
        }

        User user = byPhoneNumber.get();
        int timeAfterAccess = 1;
        if(isTimeUp(user.getVerificationCode(), timeAfterAccess, false)){
            throw new AppException("Time to send a repeat code 1 minute");
        }

        return userRepository.save(updateVerificationCode(user));
    }

//    /**
//     * Checks the verification code for a user's loginUser.
//     *
//     * @param request PhoneCodeRequest object containing the code to be verified and phone number.
//     * @return ResponseEntity containing CustomResponse with CodeDto if code verification is successful,
//     * or a bad request response with error message if user is not found, access false, max input code, invalid code
//     * time is up.
//     */
//    @Override
    public User inputPhoneCode(PhoneCodeRequest request) throws AppException {
        Optional<User> byPhone = userRepository.findByPhoneNumber(request.getPhoneNumber());
        if (byPhone.isEmpty()) {
            throw new AppException("User with this phone not found");
        }

        User user = byPhone.get();
        VerificationCode verificationCode = user.getVerificationCode();
        if(Boolean.FALSE.equals(verificationCode.getIsEntryByCode())){
            throw new AppException("There was already a code entry.");
        }

        int maxLoginAttempt = 3;
        if (!verificationCode.getCode().equals(request.getInputCode())){
            if(verificationCode.getLoginAttempt() >= maxLoginAttempt){
                throw new AppException("You've used up all your attempts");
            }
            int countAttempt = 1;
            verificationCode.setLoginAttempt(verificationCode.getLoginAttempt() + countAttempt);
            verificationCodeRepository.save(verificationCode);
            throw new AppException("The code was entered incorrectly");
        }

        int timeBeforeAccess = 5;
        if (isTimeUp(verificationCode, timeBeforeAccess, true)) {
            throw new AppException("Time is up");
        }

        verificationCode.setIsEntryByCode(false);
        verificationCodeRepository.save(verificationCode);
        return user;
    }

//    /**
//     * Registers a new user with the provided registerUser request.
//     *
//     * @param request RegistrationRequest object containing user's registerUser data.
//     * @return ResponseEntity containing CustomResponse with PhoneNumberDto if registerUser is successful,
//     * or a bad request response with error message if phone number already exists.
//     */
//    @Override
    public User registrationUser(RegistrationRequest request) throws AppException {
        if (Boolean.TRUE.equals(userRepository.existsByPhoneNumber(request.getPhoneNumber()))) {
            throw new AppException("Phone already exist " + request.getPhoneNumber());
        }

        User user = createdUser(request.getFirstName(), request.getPhoneNumber());
        user.setVerificationCode(createdVerificationCode(user));
        return userRepository.save(user);
    }

    private User updateVerificationCode(User user){
        user.getVerificationCode().setCode("2222");  //Вставить метод генерации кода
        user.getVerificationCode().setCreatedTimeCode(LocalDateTime.now());
        user.getVerificationCode().setLoginAttempt(0);
        user.getVerificationCode().setIsEntryByCode(true);
        return user;
    }

    private VerificationCode createdVerificationCode(User user){
        return VerificationCode.builder()
                .code("1111")   //Вставить метод генерации кода
                .user(user)
                .build();
    }

    private boolean isTimeUp(VerificationCode verificationCode, int minutes, boolean isBefore) {
        LocalDateTime userTimeAccess = verificationCode.getCreatedTimeCode().plusMinutes(minutes);
        if (isBefore) {
            return userTimeAccess.isBefore(LocalDateTime.now());
        }

        return userTimeAccess.isAfter(LocalDateTime.now());
    }

    private User createdUser(String firstName, String phoneNumber){
        return User.builder()
                .firstName(firstName)
                .phoneNumber(phoneNumber)
                .build();
    }
}