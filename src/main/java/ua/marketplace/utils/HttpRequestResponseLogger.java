package ua.marketplace.utils;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.constraints.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

/**
 * Component for logging HTTP requests and responses.
 * This class extends OncePerRequestFilter to ensure that it's executed once per request.
 */
@Component
public class HttpRequestResponseLogger extends OncePerRequestFilter {

    private static final Logger LOGGER = LoggerFactory.getLogger(HttpRequestResponseLogger.class);

    /**
     * Intercepts and logs HTTP requests and responses.
     *
     * @param request     The HTTP servlet request.
     * @param response    The HTTP servlet response.
     * @param filterChain The filter chain to proceed with the request.
     * @throws ServletException If an exception occurs during the filter processing.
     * @throws IOException      If an I/O exception occurs.
     */
    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {
        long startTime = System.currentTimeMillis();
        try {
            filterChain.doFilter(request, response);
        } finally {
            long duration = System.currentTimeMillis() - startTime;
            logRequestResponse(request, response, duration);
        }
    }

    private void logRequestResponse(HttpServletRequest request, HttpServletResponse response, long duration) {
        String method = request.getMethod();
        String url = request.getRequestURL().toString();
        int statusCode = response.getStatus();
        String contentType = response.getContentType();
        String remoteAddr = request.getRemoteAddr();

        LOGGER.info("Request: {} {} from {} - Response: {} {} in {} ms"
                , method, url, remoteAddr, statusCode, contentType, duration);
    }
}
