package ua.marketplace.services;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import ua.marketplace.entities.User;
import ua.marketplace.entities.VerificationCode;
import ua.marketplace.exception.AppException;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.repositoryes.VerificationCodeRepository;
import ua.marketplace.requests.PhoneCodeRequest;
import ua.marketplace.requests.PhoneNumberRequest;
import ua.marketplace.requests.RegistrationRequest;
import java.time.LocalDateTime;

/**
 * The class of service responsible for processing phone number registration and login transactions.
 */
@Service
@RequiredArgsConstructor
public class PhoneNumberRegistrationService implements IPhoneNumberRegistrationService{

    private final UserRepository userRepository;
    private final VerificationCodeRepository verificationCodeRepository;

    @Override
    public User inputPhoneNumber(PhoneNumberRequest request) throws AppException {
        User user = getUserByPhoneNumber(request.getPhoneNumber());
        checkTimeForResendingCode(user);
        return userRepository.save(updateVerificationCode(user));
    }

    private User updateVerificationCode(User user){
        user.getVerificationCode().setCode("2222");  //Вставить метод генерации кода
        user.getVerificationCode().setCreatedTimeCode(LocalDateTime.now());
        user.getVerificationCode().setLoginAttempt(0);
        user.getVerificationCode().setIsEntryByCode(true);
        return user;
    }

    private User getUserByPhoneNumber(String phoneNumber) throws AppException {
        return userRepository.findByPhoneNumber(phoneNumber)
        .orElseThrow(() -> new AppException("User with this phone not found " + phoneNumber));
    }

    private void checkTimeForResendingCode(User user) throws AppException {
        int timeAfterAccess = 1;
        if (isTimeUp(user.getVerificationCode(), timeAfterAccess, false)) {
            throw new AppException("Time to send a repeat code 1 minute");
        }
    }

    private boolean isTimeUp(VerificationCode verificationCode, int minutes, boolean isBefore) {
        LocalDateTime userTimeAccess = verificationCode.getCreatedTimeCode().plusMinutes(minutes);
        if (isBefore) {
            return userTimeAccess.isBefore(LocalDateTime.now());
        }

        return userTimeAccess.isAfter(LocalDateTime.now());
    }

    @Override
    public User inputPhoneCode(PhoneCodeRequest request) throws AppException {
        User user = getUserByPhoneNumber(request.getPhoneNumber());
        VerificationCode verificationCode = user.getVerificationCode();
        validateCodeEntry(verificationCode);
        validateCode(verificationCode, request.getInputCode());
        validateTime(verificationCode);
        resetVerificationCode(verificationCode);
        return user;
    }

    private void validateCodeEntry(VerificationCode verificationCode) throws AppException {
        if (Boolean.FALSE.equals(verificationCode.getIsEntryByCode())) {
            throw new AppException("There was already a code entry.");
        }
    }

    private void validateCode(VerificationCode verificationCode, String inputCode) throws AppException {
        if (!verificationCode.getCode().equals(inputCode)) {
            int maxLoginAttempt = 3;
            if (verificationCode.getLoginAttempt() >= maxLoginAttempt) {
                throw new AppException("You've used up all your attempts");
            }

            verificationCode.setLoginAttempt(verificationCode.getLoginAttempt() + 1);
            verificationCodeRepository.save(verificationCode);
            throw new AppException("The code was entered incorrectly");
        }
    }

    private void validateTime(VerificationCode verificationCode) throws AppException {
        int timeBeforeAccess = 5;
        if (isTimeUp(verificationCode, timeBeforeAccess, true)) {
            throw new AppException("Time is up");
        }
    }

    private void resetVerificationCode(VerificationCode verificationCode) {
        verificationCode.setIsEntryByCode(false);
        verificationCodeRepository.save(verificationCode);
    }

    @Override
    public User registrationUser(RegistrationRequest request) throws AppException {
        validatePhoneNumberNotExist(request.getPhoneNumber());
        User user = createUserWithVerificationCode(request.getFirstName(), request.getPhoneNumber());
        return userRepository.save(user);
    }

    private void validatePhoneNumberNotExist(String phoneNumber) throws AppException {
        if (Boolean.TRUE.equals(userRepository.existsByPhoneNumber(phoneNumber))) {
            throw new AppException("Phone already exists: " + phoneNumber);
        }
    }

    private User createUserWithVerificationCode(String firstName, String phoneNumber) {
        User user = createdUser(firstName, phoneNumber);
        user.setVerificationCode(createdVerificationCode(user));
        return user;
    }

    private VerificationCode createdVerificationCode(User user){
        return VerificationCode.builder()
                .code("1111")   //Вставить метод генерации кода
                .user(user)
                .build();
    }

    private User createdUser(String firstName, String phoneNumber){
        return User.builder()
                .firstName(firstName)
                .phoneNumber(phoneNumber)
                .build();
    }
}