package ua.marketplace.responses;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * A class representing a custom response with additional properties.
 *
 * @param <T> the type of the response body
 */
@Builder
@Getter
@Setter
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

    /**
     * Compares this CustomResponse with the specified object for equality.
     * Two CustomResponse objects are considered equal if they have the same success, code, message, and body.
     *
     * @param object the object to be compared for equality with this CustomResponse
     * @return true if the specified object is equal to this CustomResponse, otherwise false
     */
    @Override
    public boolean equals(Object object) {
        if (this == object) return true;
        if (object == null || getClass() != object.getClass()) return false;
        CustomResponse<?> that = (CustomResponse<?>) object;
        return code == that.code && Objects.equals(success, that.success) && Objects.equals(message, that.message) && Objects.equals(body, that.body);
    }

    /**
     * Returns a hash code value for this CustomResponse.
     *
     * @return a hash code value for this CustomResponse
     */
    @Override
    public int hashCode() {
        return Objects.hash(success, code, message, body);
    }
}