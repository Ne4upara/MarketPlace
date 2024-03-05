package ua.marketplace.advice;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.exception.ConflictException;
import ua.marketplace.utils.ErrorMessageHandler;
import ua.marketplace.exception.AppException;
import java.util.HashMap;
import java.util.Map;

@RestControllerAdvice
public class ApplicationExceptionHandler {

    @ExceptionHandler(AppException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    public Map<String, String> appException(Exception ex){
        Map<String, String> errorMap = new HashMap<>();
        errorMap.put(ErrorMessageHandler.ERROR_MESSAGE, ex.getMessage());
        return errorMap;
    }

    @ExceptionHandler(ConflictException.class)
    @ResponseStatus(HttpStatus.CONFLICT)
    public Map<String, String> conflictException(Exception ex){
        Map<String, String> errorMap = new HashMap<>();
        errorMap.put(ErrorMessageHandler.ERROR_MESSAGE, ex.getMessage());
        return errorMap;
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    public Map<String, Map<String, String>> handleInvalidArgument(MethodArgumentNotValidException ex){
        Map<String, Map<String,String>> errorResponse = new HashMap<>();
        Map<String, String> errorMap = new HashMap<>();
        ex.getBindingResult().getFieldErrors().forEach(error ->
            errorMap.put(error.getField(), error.getDefaultMessage())
        );
        errorResponse.put(ErrorMessageHandler.ERROR_MESSAGE, errorMap);
        return errorResponse;
    }

    @ExceptionHandler(ResponseStatusException.class)
    @ResponseStatus(HttpStatus.NOT_FOUND)
    public Map<String, String> handleResponseStatusException(ResponseStatusException ex) {
        String errorMessage = ex.getMessage();
        int startIndex = errorMessage.indexOf('"');
        int endIndex = errorMessage.lastIndexOf('"');
        if (startIndex != -1 && endIndex != -1 && startIndex != endIndex) {
            errorMessage = errorMessage.substring(startIndex + 1, endIndex);
        }

        Map<String, String> errorMap = new HashMap<>();
        errorMap.put(ErrorMessageHandler.ERROR_MESSAGE, errorMessage);
        return errorMap;
    }
}