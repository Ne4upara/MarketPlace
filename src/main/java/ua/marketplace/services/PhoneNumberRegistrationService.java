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

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * A service class responsible for handling phone number registration and login operations.
 */
@Service
@RequiredArgsConstructor
public class PhoneNumberRegistrationService implements IPhoneNumberRegistrationService {

    private final UserRepository userRepository;
    private final VerificationCodeRepository verificationCodeRepository;

    /**
     * Method for entering a phone number and initiating the registration process.
     *
     * @param request The request object containing the phone number registration request.
     * @return The user object that has been saved after updating the verification code.
     * @throws AppException An exception that occurs if the user is not found with the given phone number.
     */
    @Override
    public User inputPhoneNumber(PhoneNumberRequest request) {
        User user = getUserByPhoneNumber(request.phoneNumber());
        checkTimeForResendingCode(user);
        return userRepository.save(updateVerificationCode(user));
    }

    /**
     * Method for updating the verification code of a user.
     *
     * @param user The user object for which the verification code needs to be updated.
     * @return The updated user object with the updated verification code.
     */
    private User updateVerificationCode(User user) {
        user.getVerificationCode().setCode("2222");  //Вставить метод генерации кода
        user.getVerificationCode().setCreatedTimeCode(LocalDateTime.now());
        user.getVerificationCode().setLoginAttempt(0);
        user.getVerificationCode().setIsEntryByCode(Boolean.TRUE);
        return user;
    }

    /**
     * Retrieves the user by their phone number.
     *
     * @param phoneNumber The phone number of the user.
     * @return The user object with the specified phone number.
     * @throws AppException An exception that occurs if the user is not found with the specified phone number.
     */
    private User getUserByPhoneNumber(String phoneNumber) {
        return userRepository.findByPhoneNumber(phoneNumber)
                .orElseThrow(() -> new AppException("User with this phone not found " + phoneNumber));
    }

    /**
     * Checks if it's time to resend the verification code.
     *
     * @param user The user object to check for resending the verification code.
     * @throws AppException An exception indicating that it's not time to resend the code yet.
     */
    private void checkTimeForResendingCode(User user) {
        int timeAfterAccess = 1;
        if (isTimeUp(user.getVerificationCode(), timeAfterAccess, false)) {
            throw new AppException("Time to send a repeat code 1 minute");
        }
    }

    /**
     * Checks if the time is up for a specific operation.
     *
     * @param verificationCode The verification code object containing creation time.
     * @param minutes          The number of minutes to check against.
     * @param isBefore         Boolean flag indicating whether to check if the time is before the specified duration.
     * @return True if the time is up, false otherwise.
     */
    private boolean isTimeUp(VerificationCode verificationCode, int minutes, boolean isBefore) {
        LocalDateTime userTimeAccess = verificationCode.getCreatedTimeCode().plusMinutes(minutes);
        if (isBefore) {
            return userTimeAccess.isBefore(LocalDateTime.now());
        }

        return userTimeAccess.isAfter(LocalDateTime.now());
    }

    /**
     * Method for entering the verification code received via SMS.
     *
     * @param request The request object containing the verification code input.
     * @return The user object if verification is successful.
     * @throws AppException An exception that occurs if verification fails.
     */
    @Override
    public User inputPhoneCode(PhoneCodeRequest request) {
        User user = getUserByPhoneNumber(request.phoneNumber());
        VerificationCode verificationCode = user.getVerificationCode();
        validateCodeEntry(verificationCode);
        validateCode(verificationCode, request.inputCode());
        validateTime(verificationCode);
        resetVerificationCode(verificationCode);
        return user;
    }

    /**
     * Validates if the user has already entered a verification code.
     *
     * @param verificationCode The verification code object to validate.
     * @throws AppException An exception indicating that a code has already been entered.
     */
    private void validateCodeEntry(VerificationCode verificationCode) {
        if (Boolean.FALSE.equals(verificationCode.getIsEntryByCode())) {
            throw new AppException("There was already a code entry.");
        }
    }

    /**
     * Validates the entered verification code.
     *
     * @param verificationCode The verification code object to validate against.
     * @param inputCode        The input code to validate.
     * @throws AppException An exception indicating that the entered code is incorrect or maximum attempts reached.
     */
    private void validateCode(VerificationCode verificationCode, String inputCode) {
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

    /**
     * Validates if the time is not up for code verification.
     *
     * @param verificationCode The verification code object to check for time validation.
     * @throws AppException An exception indicating that the time is up for code verification.
     */
    private void validateTime(VerificationCode verificationCode) {
        int timeBeforeAccess = 5;
        if (isTimeUp(verificationCode, timeBeforeAccess, true)) {
            throw new AppException("Time is up");
        }
    }

    /**
     * Resets the verification code entry flag after successful verification.
     *
     * @param verificationCode The verification code object to reset.
     */
    private void resetVerificationCode(VerificationCode verificationCode) {
        verificationCode.setIsEntryByCode(false);
        verificationCodeRepository.save(verificationCode);
    }

    /**
     * Method for registering a new user.
     *
     * @param request The object containing the data for registering a new user.
     * @return The newly registered user object saved in the database.
     * @throws AppException An exception that occurs if a user with the specified phone number already exists.
     */
    @Override
    public User registrationUser(RegistrationRequest request) {
        validatePhoneNumberNotExist(request.phoneNumber());
        User user = createUserWithVerificationCode(request.firstName(), request.phoneNumber());
        return userRepository.save(user);
    }

    /**
     * Validates if the phone number is not already registered.
     *
     * @param phoneNumber The phone number to validate.
     * @throws AppException An exception indicating that the phone number is already registered.
     */
    private void validatePhoneNumberNotExist(String phoneNumber) {
        if (Boolean.TRUE.equals(userRepository.existsByPhoneNumber(phoneNumber))) {
            throw new AppException("Phone already exists: " + phoneNumber);
        }
    }

    /**
     * Creates a user object with a verification code.
     *
     * @param firstName   The first name of the user.
     * @param phoneNumber The phone number of the user.
     * @return The user object created with the specified data.
     */
    private User createUserWithVerificationCode(String firstName, String phoneNumber) {
        User user = createdUser(firstName, phoneNumber);
        user.setVerificationCode(createdVerificationCode(user));
        return user;
    }

    /**
     * Creates a verification code object for the given user.
     *
     * @param user The user for which the verification code needs to be created.
     * @return The verification code object created.
     */
    private VerificationCode createdVerificationCode(User user) {
        return VerificationCode.builder()
                .code("1111")   //Вставить метод генерации кода
                .user(user)
                .build();
    }

    /**
     * Creates a new user object with the given data.
     *
     * @param firstName   The first name of the user.
     * @param phoneNumber The phone number of the user.
     * @return The newly created user object.
     */
    private User createdUser(String firstName, String phoneNumber) {
        return User.builder()
                .firstName(firstName)
                .phoneNumber(phoneNumber)
                .build();
    }
}