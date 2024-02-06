package ua.marketplace.auth.registration;

import lombok.Builder;
import lombok.Data;

@Builder
@Data
public class RegistrationResponse {

    public enum Error {
        OK,
        PHONE_ALREADY_EXISTS,
        INVALID_USERNAME,
        INVALID_PASSWORD
    }

    private Error error;

    public static RegistrationResponse success() {
        return builder().error(Error.OK).build();
    }

    public static RegistrationResponse failed(Error error) {
        return builder().error(error).build();
    }
}