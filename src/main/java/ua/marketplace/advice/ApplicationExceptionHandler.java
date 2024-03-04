package ua.marketplace.advice;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.constants.ErrorMessageHandler;
import ua.marketplace.exception.AppException;
import java.util.HashMap;
import java.util.Map;

@RestControllerAdvice
public class ApplicationExceptionHandler {

    @ExceptionHandler(AppException.class)
    public ResponseEntity<Map<String, String>> appException(Exception ex){
        Map<String, String> errorMap = new HashMap<>();
        errorMap.put(ErrorMessageHandler.ERROR_MESSAGE, ex.getMessage());
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(errorMap);
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<Map<String, Map<String, String>>> handleInvalidArgument(MethodArgumentNotValidException ex){
        Map<String, Map<String,String>> errorResponse = new HashMap<>();
        Map<String, String> errorMap = new HashMap<>();
        ex.getBindingResult().getFieldErrors().forEach(error ->
            errorMap.put(error.getField(), error.getDefaultMessage())
        );
        errorResponse.put(ErrorMessageHandler.ERROR_MESSAGE, errorMap);
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(errorResponse);
    }

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
        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(errorMap);
    }
}