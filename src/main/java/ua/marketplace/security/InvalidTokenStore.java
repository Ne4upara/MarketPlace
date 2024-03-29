package ua.marketplace.security;

import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.HashSet;
import java.util.Set;

/**
 * Class for storing invalid tokens and periodically clearing them.
 */
@Component
public class InvalidTokenStore {
    private final Set<String> invalidTokens = new HashSet<>();

    /**
     * Adds an invalid token to the collection.
     *
     * @param token the invalid token to add
     */
    public void addInvalidToken(String token) {
        invalidTokens.add(token);
    }

    /**
     * Checks if the specified token is invalid.
     *
     * @param token the token to check
     * @return true if the token is invalid, false otherwise
     */
    public boolean isTokenInvalid(String token) {
        return invalidTokens.contains(token);
    }

    /**
     * Method that is automatically invoked every 24 hours to clear
     * the list of invalid tokens.
     */
    @Scheduled(fixedRate = 24 * 60 * 60 * 1000)
    public void clearInvalidTokens() {
        invalidTokens.clear();
    }
}
