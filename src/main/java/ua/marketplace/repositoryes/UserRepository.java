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

    Optional<User> findByPhoneNumber(String phone);

    Optional<User> findByCode(String code);

    Boolean existsByPhoneNumber(String phone);
}