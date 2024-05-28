package ua.marketplace.services;

import org.springframework.web.multipart.MultipartFile;

public interface Is3Service {

    String uploadFile(MultipartFile file);

    void deleteFile(String fileName);
}
