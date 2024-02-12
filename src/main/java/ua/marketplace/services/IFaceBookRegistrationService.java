package ua.marketplace.services;

import org.springframework.http.ResponseEntity;
import ua.marketplace.dto.FacebookDto;
import ua.marketplace.requests.FaceBookRequest;
import ua.marketplace.responses.CustomResponse;

public interface IFaceBookRegistrationService {

    ResponseEntity<CustomResponse<FacebookDto>> facebookOK(FaceBookRequest request);
}