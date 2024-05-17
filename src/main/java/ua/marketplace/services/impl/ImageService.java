package ua.marketplace.services.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.access.method.P;
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
import ua.marketplace.dto.ImageDto;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.ProductPhoto;
import ua.marketplace.entities.User;
import ua.marketplace.repositoryes.PhotoRepository;
import ua.marketplace.services.IImageService;
import ua.marketplace.utils.ErrorMessageHandler;
import net.coobird.thumbnailator.Thumbnails;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.*;
import java.security.Principal;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

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
    private static final String URL = "https://testingbucket00-0-1.s3.eu-central-1.amazonaws.com/";


    @Override
    public List<ProductPhoto> getPhotoLinks(List<MultipartFile> files, Product product) {
        isMaxLink(files);
        List<ProductPhoto> productPhotos = createPhotoMainPage(files.get(0), product);
        String photoLink = ErrorMessageHandler.DEFAULT_IMAGE_LINK;
        String originalFilename = "notName";
        for (int i = 0; i < files.size(); i++) {
            if(!files.get(i).getOriginalFilename().isEmpty()){
                photoLink = upLoadFile(files.get(i));
                 originalFilename = files.get(i).getOriginalFilename();
            }
            ProductPhoto productPhoto = createProductPhoto(
                    photoLink, product, originalFilename, false, i + 1);
            productPhotos.add(productPhoto);
        }
        return productPhotos;
    }

    private ProductPhoto createProductPhoto(
            String photoLink, Product product, String originalFileName, boolean isMainPage, int number) {
        return ProductPhoto.builder()
                .photoLink(photoLink)
                .product(product)
                .mainPage(isMainPage)
                .originalName(originalFileName)
                .numberPhoto(number + 1)
                .build();
    }

    private void isMaxLink(List<MultipartFile> files) {
        if (files.size() > MAX_PHOTOS_ALLOWED) {
            throw new ResponseStatusException
                    (HttpStatus.BAD_REQUEST, String.format(ErrorMessageHandler.MAX_LOAD_PHOTO));
        }
    }

    /**
     * Retrieves a list of updated product photo links based on the new photo URLs and product.
     *
     * @param files The list of new photo URLs.
     * @param product       The product associated with the photos.
     * @return A list of updated product photos.
     */
    @Override
    public List<ProductPhoto> getUpdateLinks(List<MultipartFile> files, Product product) {
        List<ProductPhoto> productPhotos = product.getPhotos().stream()
                .filter(p -> !p.isMainPage())
                .sorted(Comparator.comparingInt(ProductPhoto::getNumberPhoto))
                .collect(Collectors.toList());
        int filesSize = files.size();
        if(files.size() == 1 && (files.get(0).getOriginalFilename().isEmpty())) {
            filesSize = 0;
        }

        for (int i = 0; i < filesSize; i++) {
            MultipartFile multipartFile = files.get(i);
            if (i < productPhotos.size()) {
                ProductPhoto productPhoto = productPhotos.get(i);
                if (!productPhoto.getOriginalName().equals(multipartFile.getOriginalFilename()) && !files.isEmpty()) {
                    delFil(productPhoto.getPhotoLink());
                    String newUrl = upLoadFile(multipartFile);
                    if(i == 0){
                        ProductPhoto productPhotoMain = product.getPhotos().get(0);
                        productPhotoMain.setPhotoLink(newUrl);
                        productPhotoMain.setOriginalName(multipartFile.getOriginalFilename());
                        photoRepository.save(productPhotoMain);
                    }
                    productPhoto.setPhotoLink(newUrl);
                    productPhoto.setOriginalName(multipartFile.getOriginalFilename());
                }
            } else {
                String newUrl = upLoadFile(multipartFile);
                ProductPhoto productPhoto = createProductPhoto(
                        newUrl, product, multipartFile.getOriginalFilename(), false, i + 1);
                productPhotos.add(productPhoto);
            }
        }



        int currentSize = productPhotos.size();

        int newSize = files.size();
        if (newSize < currentSize) {
            for (int i = currentSize -1; i >= newSize; i--) {
                ProductPhoto photo = productPhotos.get(i);
                delFil(photo.getPhotoLink());
                Long id = photo.getId();
                photoRepository.deleteByPhotoId(id);
                productPhotos.remove(photo);
            }
        }

        if (filesSize == 0){
            ProductPhoto productPhoto = product.getPhotos().get(0);
            productPhoto.setPhotoLink(ErrorMessageHandler.DEFAULT_IMAGE_LINK);
            productPhoto.setOriginalName("notName");
            photoRepository.save(productPhoto);
            ProductPhoto productPhoto1 = productPhotos.get(0);
            delFil(productPhoto1.getOriginalName());
            productPhoto1.setPhotoLink(ErrorMessageHandler.DEFAULT_IMAGE_LINK);
            productPhoto1.setOriginalName("notName");
        }

        return productPhotos;
    }

    private List<ProductPhoto> createPhotoMainPage(MultipartFile files, Product product) {
        List<ProductPhoto> productPhotos = new ArrayList<>();
        if(files.isEmpty()) {
            productPhotos.add(
                    createProductPhoto(
                            ErrorMessageHandler.DEFAULT_IMAGE_LINK, product, "notName",true, 0));
            return productPhotos;
        }
        String urlForMAinPage = upLoadFile(files); //reSize(file); // -> resize -> upload -> return string
            productPhotos.add(createProductPhoto(
                    urlForMAinPage, product, files.getOriginalFilename(),true, 0));

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
        if (newSize + 1 < currentSize) {
//            if (newSize == 1) newSize = 2;
            for (int i = currentSize - 1; i >= newSize; i--) {
                ProductPhoto photo = listPhoto.get(i);
                Long id = photo.getId();
                photoRepository.deleteByPhotoId(id);
                listPhoto.remove(photo);
            }
        }
    }

    public String upLoadFile(MultipartFile file) {
        String randomName = utilsService.getRandomName() + file.getOriginalFilename();

        try {
            s3Client().putObject(PutObjectRequest.builder()
                    .bucket(BUCKET)
                    .key(randomName)
                    .acl(ObjectCannedACL.PUBLIC_READ)
                    .build(), RequestBody.fromInputStream(file.getInputStream(), file.getSize()));
        } catch (IOException e) {
            throw new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE);
        }

        return URL + randomName;
    }

    public void deleteFile(List<ProductPhoto> photos){
        for (ProductPhoto photo : photos) {
            s3Client().deleteObject(DeleteObjectRequest.builder()
                    .bucket(BUCKET)
                    .key(photo.getPhotoLink().replace(URL, ""))
                    .build());
        }
    }

    private void delFil(String fileName){
        s3Client().deleteObject(DeleteObjectRequest.builder()
                .bucket(BUCKET)
                .key(fileName.replace(URL, ""))
                .build());
    }

    private S3Client s3Client(){
        return S3Client.builder()
                .region(Region.EU_CENTRAL_1)
                .credentialsProvider(EnvironmentVariableCredentialsProvider.create())
                .build();
    }



//    public InputStream resizeImageTo700KB(MultipartFile file) throws IOException {
//        // Прочитать изображение в байтовый массив
//        BufferedImage image = ImageIO.read(file.getInputStream());
//
//        // Уменьшить размер и качество изображения до 700 КБайт с помощью библиотеки Thumbnailator
//        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
//        Thumbnails.of(image)
//                .size(700, 700)  // Указать размер
//                .outputQuality(0.8) // Указать качество (от 0.0 до 1.0)
//                .outputFormat("jpg") // Указать формат изображения (можно изменить на другой, если нужно)
//                .toOutputStream(outputStream);
//
//        // Вернуть уменьшенное изображение в виде InputStream
//        return new ByteArrayInputStream(outputStream.toByteArray());
//    }


}
