package ua.marketplace.security;

import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationConfiguration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

/**
 * Configuration class for security settings in the application.
 * This class defines various beans and configurations for security-related features, such as password encoding,
 * CORS, and the security filter chain.
 */
@Configuration
@EnableWebSecurity
@RequiredArgsConstructor
public class SecurityConfig {

    // Injecting dependencies
    private final JwtUtil jwtUtil;
    private final CustomUserDetailsService userDetailsService;
    private final AuthenticationConfiguration authenticationConfiguration;

    // Defining a bean for password encoder
    /**
     * Configures a bean for password encoding using BCryptPasswordEncoder.
     * This bean is used for encoding and matching passwords in the application.
     *
     * @return the BCryptPasswordEncoder instance
     */
    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

    // Configuring the security filter chain
    /**
     * Configures the security filter chain with various security settings.
     * This filter chain defines how the application handles authentication and authorization.
     *
     * @param http the HttpSecurity object to configure
     * @return the configured security filter chain
     * @throws Exception if an error occurs during configuration
     */
    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {

        // Configuring various security settings, such as CSRF, CORS, and authorization
        http
                .csrf(AbstractHttpConfigurer::disable) // Disabling CSRF protection
                .cors(c -> corsConfigurationSource()) // Configuring CORS
                .authorizeHttpRequests(authorize -> authorize
                        .requestMatchers("/v1/auth/**",
                                "/swagger/**",
                                "/swagger-ui/**",
                                "/v3/api-docs/**",
                                "/swagger-resources/**",
                                "/webjars/**",
                                "/v1/products/s/**",
                                "/v1/categories/**",
                                "/actuator/**"
                        ).permitAll() // Permitting access to specific endpoints for all users
                        .anyRequest().authenticated() // Requiring authentication for any other requests
                )
                .sessionManagement(session -> session
                        .sessionCreationPolicy(SessionCreationPolicy.ALWAYS) // Always creating a new session
                )
                .addFilterBefore(
                        new JwtRequestFilter(authenticationManager(), userDetailsService, jwtUtil),
                        UsernamePasswordAuthenticationFilter.class // Adding a custom filter before the UsernamePasswordAuthenticationFilter
                );

        return http.build(); // Building and returning the configured SecurityFilterChain
    }

    // Defining a bean for CORS configuration source
    /**
     * Configures a bean for CORS configuration source.
     * This bean is used to define which origins, methods, and headers are allowed for cross-origin requests.
     *
     * @return the CorsConfigurationSource instance
     */
    @Bean
    public CorsConfigurationSource corsConfigurationSource() {

        CorsConfiguration configuration = new CorsConfiguration();
        // Allowing all origins, methods, and headers for simplicity
        configuration.addAllowedOrigin("*");
        configuration.addAllowedMethod("*");
        configuration.addAllowedHeader("*");

        UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        // Applying the configuration to all requests
        source.registerCorsConfiguration("/**", configuration);
        return source;
    }

    // Defining a bean for authentication manager
    /**
     * Configures a bean for the authentication manager.
     * This bean is responsible for authenticating users based on the provided credentials.
     *
     * @return the AuthenticationManager instance
     * @throws Exception if an error occurs during authentication manager creation
     */
    @Bean
    public AuthenticationManager authenticationManager() throws Exception {
        return authenticationConfiguration.getAuthenticationManager();
    }
}
