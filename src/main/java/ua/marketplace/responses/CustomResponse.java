package ua.marketplace.responses;

import lombok.*;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;

/**
 * A class representing a custom response with additional properties.
 *
 * @param <T> the type of the response body
 */
@Builder
@Getter
@Setter
@EqualsAndHashCode(exclude = "timestamp")
@ToString
public class CustomResponse<T> {

    private LocalDateTime timestamp;
    private Boolean success;
    private int code;
    private List<String> message;
    private T body;

    /**
     * Creates a successful response with a body and status code.
     *
     * @param body the response body
     * @param code the status code of the response
     * @param <T>  the type of the response body
     * @return a successful response with the provided data
     */
    public static <T> CustomResponse<T> successfully(T body, int code) {
        return CustomResponse
                .<T>builder()
                .timestamp(LocalDateTime.now())
                .success(true)
                .code(code)
                .message(Collections.singletonList("OK"))
                .body(body)
                .build();
    }

    /**
     * Creates a failed response with an error message and status code.
     *
     * @param message the error messages
     * @param code    the status code of the response
     * @param <T>     the type of the response body
     * @return a failed response with the error message
     */
    public static <T> CustomResponse<T> failed(List<String> message, int code) {
        return CustomResponse
                .<T>builder()
                .timestamp(LocalDateTime.now())
                .success(false)
                .code(code)
                .message(message)
                .build();
    }
}