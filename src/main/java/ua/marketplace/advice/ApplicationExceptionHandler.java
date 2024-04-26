package ua.marketplace.advice;

import io.swagger.v3.oas.annotations.Hidden;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.utils.ErrorMessageHandler;

import java.util.HashMap;
import java.util.Map;

/**
 * Global exception handler for the application.
 * This class handles exceptions thrown during request processing and provides appropriate responses.
 */
@RestControllerAdvice
public class ApplicationExceptionHandler {

    /**
     * Handles MethodArgumentNotValidException thrown when argument annotated with @Valid failed validation.
     *
     * @param ex The MethodArgumentNotValidException to handle.
     * @return A map containing field names and their corresponding error messages.
     */
    @Hidden
    @ExceptionHandler(MethodArgumentNotValidException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    public Map<String, Map<String, String>> handleInvalidArgument(MethodArgumentNotValidException ex) {
        Map<String, Map<String, String>> errorResponse = new HashMap<>();
        Map<String, String> errorMap = new HashMap<>();
        ex.getBindingResult().getFieldErrors().forEach(error ->
                errorMap.put(error.getField(), error.getDefaultMessage())
        );
        errorResponse.put(ErrorMessageHandler.ERROR_MESSAGE, errorMap);
        return errorResponse;
    }

    /**
     * Handles ResponseStatusException thrown when a request is unable to be processed due to incorrect status.
     *
     * @param ex The ResponseStatusException to handle.
     * @return A ResponseEntity with an error message and appropriate HTTP status.
     */
    @Hidden
    @ExceptionHandler(ResponseStatusException.class)
    public ResponseEntity<Map<String, String>> handleResponseStatusException(ResponseStatusException ex) {
        String errorMessage = ex.getMessage();
        int startIndex = errorMessage.indexOf('"');
        int endIndex = errorMessage.lastIndexOf('"');
        if (startIndex != -1 && endIndex != -1 && startIndex != endIndex) {
            errorMessage = errorMessage.substring(startIndex + 1, endIndex);
        }

        Map<String, String> errorMap = new HashMap<>();
        errorMap.put(ErrorMessageHandler.ERROR_MESSAGE, errorMessage);
        return ResponseEntity.status(ex.getStatusCode()).body(errorMap);
    }

}
