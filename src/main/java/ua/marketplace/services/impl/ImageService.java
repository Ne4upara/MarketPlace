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
import software.amazon.awssdk.services.s3.model.ObjectCannedACL;
import software.amazon.awssdk.services.s3.model.PutObjectRequest;
import ua.marketplace.dto.ImageDto;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.ProductPhoto;
import ua.marketplace.repositoryes.PhotoRepository;
import ua.marketplace.services.IImageService;
import ua.marketplace.utils.ErrorMessageHandler;
import net.coobird.thumbnailator.Thumbnails;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Implementation of the {@link IImageService} interface for managing images associated with products.
 */
@Service
@RequiredArgsConstructor
public class ImageService implements IImageService {

    private final PhotoRepository photoRepository;
    private final UtilsService utilsService;
    private static final int MAX_PHOTOS_ALLOWED = 8;
    private static final String BUCKET = "testingbucket00-0-1";

    /**
     * Retrieves a list of product photos based on the provided photo URLs and product.
     *
     * @param photos  The list of photo URLs.
     * @param product The product associated with the photos.
     * @return A list of product photos.
     */
    @Override
    public List<ProductPhoto> getPhotoLinks(List<String> photos, Product product) {
        List<ProductPhoto> productPhotos = handleEmptyNewPhotoLinks(photos, product);

        for (int i = 0; i < photos.size(); i++) {
            String photoLink = photos.get(i);
            ProductPhoto productPhoto = createProductPhoto(photoLink, product, i == 0);
            productPhotos.add(productPhoto);
        }
        return productPhotos;
    }

    private ProductPhoto createProductPhoto(String photoLink, Product product, boolean isMainPage) {
        return ProductPhoto.builder()
                .photoLink(photoLink)
                .product(product)
                .mainPage(isMainPage)
                .build();
    }

    private void isMaxLink(int size) {
        if (size > MAX_PHOTOS_ALLOWED) {
            throw new ResponseStatusException
                    (HttpStatus.BAD_REQUEST, String.format(ErrorMessageHandler.MAX_LOAD_PHOTO));
        }
    }

    /**
     * Retrieves a list of updated product photo links based on the new photo URLs and product.
     *
     * @param newPhotoLinks The list of new photo URLs.
     * @param product       The product associated with the photos.
     * @return A list of updated product photos.
     */
    @Override
    public List<ProductPhoto> getUpdateLinks(List<String> newPhotoLinks, Product product) {
        List<ProductPhoto> productPhotos = handleEmptyNewPhotoLinks(newPhotoLinks, product);

        for (int i = 0; i < newPhotoLinks.size(); i++) {
            String photoLink = newPhotoLinks.get(i);
            if (i < product.getPhotos().size()) {
                ProductPhoto existingPhoto = product.getPhotos().get(i);
                existingPhoto.setPhotoLink(photoLink);
                productPhotos.add(existingPhoto);
            } else {
                ProductPhoto newPhoto = createProductPhoto(photoLink, product, i == 0);
                productPhotos.add(newPhoto);
            }
        }

        return productPhotos;
    }

    private List<ProductPhoto> handleEmptyNewPhotoLinks(List<String> newPhotoLinks, Product product) {
        List<ProductPhoto> productPhotos = new ArrayList<>();
        int newSize = newPhotoLinks.size();
        isMaxLink(newSize);
        if (newSize == 0) {
            productPhotos.add(createProductPhoto(ErrorMessageHandler.DEFAULT_IMAGE_LINK, product, true));
            return productPhotos;
        }
        return productPhotos;
    }

    /**
     * Deletes excess photos for the product based on the new size.
     *
     * @param newSize The new size of the photos.
     * @param product The product for which to delete excess photos.
     */
    @Override
    public void deleteExcessPhotos(int newSize, Product product) {
        int currentSize = product.getPhotos().size();
        List<ProductPhoto> listPhoto = product.getPhotos();
        if (newSize < currentSize) {

            for (int i = currentSize - 1; i >= newSize; i--) {
                ProductPhoto photo = listPhoto.get(i);
                Long id = photo.getId();
                photoRepository.deleteByPhotoId(id);
                listPhoto.remove(photo);
            }
        }
    }

    public ImageDto upLoadFile(List<MultipartFile> files, Principal principal, Long id) {
        utilsService.getUserByPrincipal(principal);
        List<String> uploadFiles = new ArrayList<>();
        S3Client s3Client = S3Client.builder()
                .region(Region.EU_CENTRAL_1)
                .credentialsProvider(EnvironmentVariableCredentialsProvider.create())
                .build();
        try {
            for (MultipartFile file : files) {
                String randomName = utilsService.getRandomName() + file.getName();

                s3Client.putObject(PutObjectRequest.builder()
                        .bucket(BUCKET)
                        .key(randomName + ".jpg")
                        .acl(ObjectCannedACL.PUBLIC_READ)
                        .build(), RequestBody.fromInputStream(file.getInputStream(), file.getSize()));
                uploadFiles.add("https://testingbucket00-0-1.s3.eu-central-1.amazonaws.com/" + randomName + ".jpg");
            }
        } catch (IOException e) {
            throw new ResponseStatusException(HttpStatus.CONFLICT, String.format(ErrorMessageHandler.INVALID_CATEGORY, files));
        }
        return new ImageDto(uploadFiles);
    }

    public InputStream resizeImageTo700KB(MultipartFile file) throws IOException {
        // Прочитать изображение в байтовый массив
        BufferedImage image = ImageIO.read(file.getInputStream());

        // Уменьшить размер и качество изображения до 700 КБайт с помощью библиотеки Thumbnailator
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        Thumbnails.of(image)
                .size(700, 700)  // Указать размер
                .outputQuality(0.8) // Указать качество (от 0.0 до 1.0)
                .outputFormat("jpg") // Указать формат изображения (можно изменить на другой, если нужно)
                .toOutputStream(outputStream);

        // Вернуть уменьшенное изображение в виде InputStream
        return new ByteArrayInputStream(outputStream.toByteArray());
    }


}
