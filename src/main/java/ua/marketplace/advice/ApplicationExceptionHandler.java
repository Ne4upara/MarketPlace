package ua.marketplace.advice;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.*;
import ua.marketplace.exception.AppException;
import java.util.HashMap;
import java.util.Map;

@RestControllerAdvice
public class ApplicationExceptionHandler {

    @ExceptionHandler(AppException.class)
    public ResponseEntity<Map<String, Map<String, String>>> appException(Exception ex){
        Map<String, Map<String,String>> errorResponse = new HashMap<>();
        Map<String, String> errorMap = new HashMap<>();
        errorMap.put("Message", ex.getMessage());
        errorResponse.put("errorMessage", errorMap);
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(errorResponse);
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<Map<String, Map<String, String>>> handleInvalidArgument(MethodArgumentNotValidException ex){
        Map<String, Map<String,String>> errorResponse = new HashMap<>();
        Map<String, String> errorMap = new HashMap<>();
        ex.getBindingResult().getFieldErrors().forEach(error ->
            errorMap.put(error.getField(), error.getDefaultMessage())
        );
        errorResponse.put("errorMessage", errorMap);
        return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(errorResponse);
    }
}