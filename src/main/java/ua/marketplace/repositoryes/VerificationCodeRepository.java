package ua.marketplace.repositoryes;

import org.springframework.data.jpa.repository.JpaRepository;
import ua.marketplace.entities.VerificationCode;

/**
 * This interface extends the JpaRepository interface to provide CRUD (Create, Read, Update, Delete) operations for VerificationCode entities.
 * By implementing this interface, we can easily perform database operations on VerificationCode objects without having to write any implementation code.
 *
 * @param <VerificationCode> The type of entity that this repository manages (in this case, VerificationCode).
 * @param <Long> The type of the primary key of the entity (in this case, Long).
 */
public interface VerificationCodeRepository extends JpaRepository<VerificationCode, Long> {
    // CRUD operations for VerificationCode entities are automatically implemented by Spring Data JPA.
    // These operations include:
    // - save(): saves a given VerificationCode entity
    // - findAll(): returns all VerificationCode entities
    // - findById(): returns a single VerificationCode entity by its id
    // - deleteById(): deletes a VerificationCode entity by its id
    // - and many more...
}
