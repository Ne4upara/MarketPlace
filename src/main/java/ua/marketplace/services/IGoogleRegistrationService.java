package ua.marketplace.services;

import org.springframework.http.ResponseEntity;
import ua.marketplace.dto.GoogleDto;
import ua.marketplace.requests.GoogleRequest;
import ua.marketplace.responses.CustomResponse;

public interface IGoogleRegistrationService {

    ResponseEntity<CustomResponse<GoogleDto>> googleOK(GoogleRequest request);
}