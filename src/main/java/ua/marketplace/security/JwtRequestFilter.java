package ua.marketplace.security;

import io.jsonwebtoken.JwtException;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpStatus;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.security.web.authentication.www.BasicAuthenticationFilter;

/**
 * A filter responsible for processing JWT authentication requests.
 * This filter intercepts incoming requests, extracts JWT tokens from them,
 * and validates the tokens to authenticate users.
 */
public class JwtRequestFilter extends BasicAuthenticationFilter {

    private final JwtUtil jwtUtil;
    private final UserDetailsService userDetailsService;

    /**
     * Constructs a new JwtRequestFilter instance with the given AuthenticationManager,
     * UserDetailsService, and JwtUtil.
     *
     * @param authenticationManager the AuthenticationManager for handling authentication
     * @param userDetailsService   the UserDetailsService for loading user details
     * @param jwtUtil             the JwtUtil for handling JWT token operations
     */
    public JwtRequestFilter(AuthenticationManager authenticationManager, UserDetailsService userDetailsService,
                            JwtUtil jwtUtil) {

        super(authenticationManager);
        this.jwtUtil = jwtUtil;
        this.userDetailsService = userDetailsService;
    }

    /**
     * Filters incoming requests to extract and validate JWT tokens for authentication.
     *
     * @param request  the incoming HttpServletRequest
     * @param response the HttpServletResponse
     * @param chain    the FilterChain for invoking the next filter in the chain
     * @throws IOException      if an I/O error occurs during filtering
     * @throws ServletException if a servlet-related error occurs during filtering
     */
    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain chain)
            throws IOException, ServletException {

        final String authorizationHeader = request.getHeader("Authorization");

        String username = null;
        String jwt = null;

        // Check if the request contains an Authorization header starting with "Bearer "
        if (authorizationHeader != null && authorizationHeader.startsWith("Bearer ")) {
            jwt = authorizationHeader.substring(7); // Extract the JWT token
            try {
                // Extract the username from the JWT token
                username = jwtUtil.extractUsername(jwt);
            } catch (JwtException e) {
                // If the JWT token has expired, set the response status to 418 (I'm a teapot)
                // and write an error message
                response.setStatus(HttpStatus.I_AM_A_TEAPOT.value());
                response.getWriter().write("JWT token has expired");
                response.setContentType("application/json");
                return;
            }
        }

        // If a valid username and JWT token are present, authenticate the user
        if (username != null && SecurityContextHolder.getContext().getAuthentication() == null) {

            UserDetails userDetails = this.userDetailsService.loadUserByUsername(username);

            if (Boolean.TRUE.equals(jwtUtil.validateToken(jwt, userDetails))) {
                UsernamePasswordAuthenticationToken usernamePasswordAuthenticationToken =
                        new UsernamePasswordAuthenticationToken(userDetails, null,
                                userDetails.getAuthorities());
                usernamePasswordAuthenticationToken
                        .setDetails(new WebAuthenticationDetailsSource().buildDetails(request));

                SecurityContextHolder.getContext().setAuthentication(usernamePasswordAuthenticationToken);
            }
        }

        // Invoke the next filter in the chain
        chain.doFilter(request, response);
    }
}
