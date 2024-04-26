package ua.marketplace.security;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;
import lombok.RequiredArgsConstructor;
import ua.marketplace.entities.BlackListToken;
import ua.marketplace.repositoryes.BlackListRepository;

import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Component;

import java.security.Key;
import java.util.*;
import java.util.function.Function;

/**
 * Utility class for handling JSON Web Tokens (JWT).
 */
@Component
@RequiredArgsConstructor
public class JwtUtil {

    private static final String SECRET_KEY = System.getenv("JWT_SECRET");
    private final UserDetailsService userDetailsService;
    private final BlackListRepository blackList;

    /**
     * Generates a JWT token for the given username.
     *
     * @param username the username for which to generate the token
     * @return the generated JWT token
     */
    public String generateToken(String username) {
        Map<String, Object> claims = new HashMap<>();
        return createToken(claims, username);
    }

    /**
     * Validates a JWT token against the provided UserDetails.
     *
     * @param token       the JWT token to validate
     * @param userDetails the UserDetails to validate against
     * @return true if the token is valid for the user, false otherwise
     */
    public Boolean validateToken(String token, UserDetails userDetails) {
        final String username = extractUsername(token);
        return (username.equals(userDetails.getUsername())
                && !isTokenExpired(token)
                && !isTokenInBlackList(token));
    }

    private boolean isTokenInBlackList(String token) {
        return blackList.existsByToken(token);
    }

    /**
     * Extracts the username from the given JWT token.
     *
     * @param token the JWT token from which to extract the username
     * @return the username extracted from the token
     */
    public String extractUsername(String token) {
        return extractClaim(token, Claims::getSubject);
    }

    /**
     * Adds the provided token to the blacklist.
     *
     * @param token the JWT token to blacklist
     */
    public void killToken(String token) {
        BlackListToken blackListToken = BlackListToken.builder()
                .token(token)
                .expiredTokens(extractExpiration(token))
                .build();
        blackList.save(blackListToken);
    }

    private Date extractExpiration(String token) {
        return extractClaim(token, Claims::getExpiration);
    }

    private <T> T extractClaim(String token, Function<Claims, T> claimsResolver) {
        final Claims claims = extractAllClaims(token);
        return claimsResolver.apply(claims);
    }

    private Claims extractAllClaims(String token) {
        return Jwts.parserBuilder()
                .setSigningKey(getSigningKey())
                .build()
                .parseClaimsJws(token)
                .getBody();
    }

    private Key getSigningKey() {
        byte[] keyBytes = Decoders.BASE64.decode(SECRET_KEY);
        return Keys.hmacShaKeyFor(keyBytes);
    }

    private Boolean isTokenExpired(String token) {
        return extractExpiration(token).before(new Date());
    }

    private String createToken(Map<String, Object> claims, String subject) {
        return Jwts.builder()
                .setClaims(claims)
                .setSubject(userDetailsService.loadUserByUsername(subject).getUsername())
                .setIssuedAt(new Date(System.currentTimeMillis()))
                .setExpiration(new Date(System.currentTimeMillis() + (1000 * 60 * 60 * 24 * 10))) // 10 days
                .signWith(getSigningKey(), SignatureAlgorithm.HS256)
                .compact();
    }

    /**
     * Clears expired tokens from the blacklist.
     * This method is scheduled to run at 3:00 AM every day.
     * Expired tokens are removed from the blacklist to optimize memory usage.
     */
//    @Scheduled(cron = "0 0 3 * * *") //В 3 часа ночи каждый день.
    @Scheduled(fixedDelay = 6 * 60 * 60 * 1000) // каждые 6 часов
    public void clearBlackListTokens() {
        List<BlackListToken> expiredTokens = blackList.findAllExpiredTokens();
        blackList.deleteAll(expiredTokens);

    }

}
