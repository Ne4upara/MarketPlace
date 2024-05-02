package ua.marketplace.repositoryes;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import ua.marketplace.entities.BlackListToken;

/**
 * Repository interface for accessing and managing BlackListToken entities in the database.
 * Extends JpaRepository for basic CRUD operations.
 */
public interface BlackListRepository extends JpaRepository <BlackListToken, Long> {

    /**
     * Checks if a blacklisted token exists by its token value.
     *
     * @param token The token value to check for existence.
     * @return True if a blacklisted token with the given value exists, otherwise false.
     */
    Boolean existsByToken(String token);

    /**
     * Retrieves all blacklisted tokens that have expired.
     *
     * @return A list of all expired blacklisted tokens.
     */
    @Query("SELECT b FROM BlackListToken b WHERE b.expiredTokens < CURRENT_TIMESTAMP")
    List<BlackListToken> findAllExpiredTokens();
}