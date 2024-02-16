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
     * Retrieves an Optional containing the user with the specified phone number,
     * or an empty Optional if no such user exists.
     *
     * @param phone the phone number of the user to retrieve
     * @return an Optional containing the user with the specified phone number, if found
     */
    Optional<User> findByPhoneNumber(String phone);

    /**
     * Checks if a user with the specified phone number exists in the database.
     *
     * @param phone the phone number to check
     * @return true if a user with the specified phone number exists, false otherwise
     */
    Boolean existsByPhoneNumber(String phone);
}