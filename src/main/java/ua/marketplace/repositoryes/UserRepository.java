package ua.marketplace.repositoryes;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import ua.marketplace.entities.User;

import java.util.Optional;

/**
 * A repository interface for accessing user data in the database.
 */
@Repository
public interface UserRepository extends JpaRepository<User, Long> {

    /**
     * Retrieves a user by their phone number.
     *
     * @param phone Phone number of the user.
     * @return Optional containing the user if found, otherwise empty.
     */
    Optional<User> findByPhoneNumber(String phone);

    /**
     * Retrieves a user by their verification code.
     *
     * @param code Verification code associated with the user's phone number.
     * @return Optional containing the user if found, otherwise empty.
     */
    Optional<User> findBySmsCode(String code);

    /**
     * Checks if a user exists with the given phone number.
     *
     * @param phone Phone number to check for existence.
     * @return True if a user exists with the given phone number, false otherwise.
     */
    Boolean existsByPhoneNumber(String phone);
}
