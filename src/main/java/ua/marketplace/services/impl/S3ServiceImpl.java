package ua.marketplace.services.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.server.ResponseStatusException;
import software.amazon.awssdk.auth.credentials.EnvironmentVariableCredentialsProvider;
import software.amazon.awssdk.core.sync.RequestBody;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.DeleteObjectRequest;
import software.amazon.awssdk.services.s3.model.ObjectCannedACL;
import software.amazon.awssdk.services.s3.model.PutObjectRequest;
import ua.marketplace.services.S3Service;
import ua.marketplace.utils.ErrorMessageHandler;

import java.io.IOException;

@Service
@RequiredArgsConstructor
public class S3ServiceImpl implements S3Service {

    private final UtilsService utilsService;
    private static final String BUCKET = "testingbucket00-0-1";
    private static final String URL = "https://testingbucket00-0-1.s3.eu-central-1.amazonaws.com/";

    private S3Client s3Client() {
        return S3Client.builder()
                .region(Region.EU_CENTRAL_1)
                .credentialsProvider(EnvironmentVariableCredentialsProvider.create())
                .build();
    }

    @Override
    public String uploadFile(MultipartFile file) {
        String randomName = utilsService.getRandomName() + file.getOriginalFilename();
        try {
            s3Client().putObject(PutObjectRequest.builder()
                    .bucket(BUCKET)
                    .key(randomName)
                    .acl(ObjectCannedACL.PUBLIC_READ)
                    .build(), RequestBody.fromInputStream(file.getInputStream(), file.getSize()));
        } catch (IOException e) {
            // NOPMD: Игнорирование предупреждения PMD для этого блока кода
            throw new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE, ErrorMessageHandler.FAILED_UPLOAD); //NOPMD
        }
        return URL + randomName;
    }

    @Override
    public void deleteFile(String fileName) {
        s3Client().deleteObject(DeleteObjectRequest.builder()
                .bucket(BUCKET)
                .key(fileName.replace(URL, ""))
                .build());
    }
}