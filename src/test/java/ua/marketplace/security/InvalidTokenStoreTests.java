package ua.marketplace.security;

import org.junit.jupiter.api.Test;



import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@SuppressWarnings("PMD")
@SpringBootTest
class InvalidTokenStoreTests {

    @Autowired
    private InvalidTokenStore invalidTokenStore;

    @Test
    void testAddInvalidToken() {
        // Given
        String token = "invalid_token";

        // When
        invalidTokenStore.addInvalidToken(token);

        // Then
        assertTrue(invalidTokenStore.isTokenInvalid(token));
    }

    @Test
    void testClearInvalidTokens() {
        // Given
        String token1 = "invalid_token_1";
        String token2 = "invalid_token_2";

        invalidTokenStore.addInvalidToken(token1);
        invalidTokenStore.addInvalidToken(token2);

        assertTrue(invalidTokenStore.isTokenInvalid(token1));
        assertTrue(invalidTokenStore.isTokenInvalid(token2));

        // When
        invalidTokenStore.clearInvalidTokens();

        // Then
        assertFalse(invalidTokenStore.isTokenInvalid(token1));
        assertFalse(invalidTokenStore.isTokenInvalid(token2));
    }
}
