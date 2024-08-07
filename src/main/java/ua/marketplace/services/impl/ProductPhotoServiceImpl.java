package ua.marketplace.services.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.ProductPhoto;
import ua.marketplace.repositoryes.PhotoRepository;
import ua.marketplace.services.ProductPhotoService;
import ua.marketplace.utils.ErrorMessageHandler;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link ProductPhotoService} interface for managing images associated with products.
 */
@Service
@RequiredArgsConstructor
public class ProductPhotoServiceImpl implements ProductPhotoService {

    private final PhotoRepository photoRepository;
    private final S3ServiceImpl s3Service;
    private static final int MAX_PHOTOS_ALLOWED = 8;
    private static final String DEFAULT_NAME = "noName";
    public static final String DEFAULT_IMAGE_LINK =
            "https://i.pinimg.com/1200x/9f/ab/e5/9fabe5f90ca53f9a86306203f517f9fd.jpg";


    @Override
    public List<ProductPhoto> getPhotoLinks(List<MultipartFile> files, Product product) {
        validateImageFiles(files);
        validateMaxLink(files.size());
        List<ProductPhoto> productPhotos = createPhotoMainPage(files.get(0), product);
        for (int i = 0; i < files.size(); i++) {
            MultipartFile file = files.get(i);
            String photoLink = Objects.requireNonNull(file.getOriginalFilename()).isEmpty() ?
                    DEFAULT_IMAGE_LINK : s3Service.uploadFile(file) ;
            ProductPhoto productPhoto = createProductPhoto(
                    photoLink, product, file.getOriginalFilename(), false, i + 1);
            productPhotos.add(productPhoto);
        }
        return productPhotos;
    }

    private void validateImageFiles(List<MultipartFile> files) {
        for (MultipartFile file : files) {
            String fileName = file.getOriginalFilename();
            if (!isImageFile(fileName)) {
                throw new ResponseStatusException(
                        HttpStatus.BAD_REQUEST,
                        String.format(ErrorMessageHandler.INCORRECT_FILE_FORMAT,file.getOriginalFilename()));
            }
        }
    }

    private boolean isImageFile(String fileName) {
        if (fileName == null) {
            return false;
        }

        if(fileName.isEmpty()){
            return true;
        }
        String lowerCaseFileName = fileName.toLowerCase(Locale.ENGLISH);
        return lowerCaseFileName.endsWith(".jpeg") ||
                lowerCaseFileName.endsWith(".jpg") ||
                lowerCaseFileName.endsWith(".png");
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

    private void validateMaxLink(int size) {
        if (size > MAX_PHOTOS_ALLOWED) {
            throw new ResponseStatusException
                    (HttpStatus.BAD_REQUEST, String.format(ErrorMessageHandler.MAX_LOAD_PHOTO));
        }
    }

    @Override
    public List<ProductPhoto> getUpdateLinks(List<MultipartFile> files, Product product) {

        List<ProductPhoto> productPhotos = product.getPhotos().stream()
                .filter(p -> !p.isMainPage())
                .sorted(Comparator.comparingInt(ProductPhoto::getNumberPhoto))
                .collect(Collectors.toList());

        int filesSize = adjustFileSize(files);

        for (int i = 0; i < filesSize; i++) {
            MultipartFile multipartFile = files.get(i);
            if (i < productPhotos.size()) {
                updatePhoto(productPhotos.get(i), multipartFile, i == 0 ? product.getPhotos().get(0) : null);
            } else {
                validateMaxLink(productPhotos.size());
                addNewPhoto(product, productPhotos, multipartFile, i);
            }
        }

        List<ProductPhoto> newProductPhotos = deleteExcessPhotos(files.size(), productPhotos);

        if (filesSize == 0){
            resetMainPhoto(product.getPhotos().get(0));
            ProductPhoto productPhoto1 = newProductPhotos.get(0);
            s3Service.deleteFile(productPhoto1.getPhotoLink());
            productPhoto1.setPhotoLink(DEFAULT_IMAGE_LINK);
            productPhoto1.setOriginalName(DEFAULT_NAME);
        }

        return newProductPhotos;
    }

    private void updatePhoto(ProductPhoto productPhoto, MultipartFile file, ProductPhoto mainPhoto) {
        if (!productPhoto.getOriginalName().equals(file.getOriginalFilename()) && !file.isEmpty()) {
            s3Service.deleteFile(productPhoto.getPhotoLink());
            String newUrl = s3Service.uploadFile(file);
            if (mainPhoto != null) {
                mainPhoto.setPhotoLink(newUrl);
                mainPhoto.setOriginalName(file.getOriginalFilename());
                photoRepository.save(mainPhoto);
            }
            productPhoto.setPhotoLink(newUrl);
            productPhoto.setOriginalName(file.getOriginalFilename());
        }
    }

    private void addNewPhoto(Product product, List<ProductPhoto> productPhotos, MultipartFile file, int index) {
        String newUrl = s3Service.uploadFile(file);
        ProductPhoto productPhoto = createProductPhoto(newUrl, product, file.getOriginalFilename(), false, index + 1);
        productPhotos.add(productPhoto);
    }

    private int adjustFileSize(List<MultipartFile> files) {
        if (files.size() == 1 && Objects.requireNonNull(files.get(0).getOriginalFilename()).isEmpty()) {
            return 0;
        }
        return files.size();
    }

    private void resetMainPhoto(ProductPhoto productPhoto) {
        s3Service.deleteFile(productPhoto.getPhotoLink());
        productPhoto.setPhotoLink(DEFAULT_IMAGE_LINK);
        productPhoto.setOriginalName(DEFAULT_NAME);
        photoRepository.save(productPhoto);
    }

    private List<ProductPhoto> createPhotoMainPage(MultipartFile files, Product product) {
        List<ProductPhoto> productPhotos = new ArrayList<>();
        if(files.isEmpty()) {
            productPhotos.add(
                    createProductPhoto(
                            DEFAULT_IMAGE_LINK, product, DEFAULT_NAME,true, 0));
            return productPhotos;
        }
        String urlForMAinPage = s3Service.uploadFile(files);
            productPhotos.add(createProductPhoto(
                    urlForMAinPage, product, files.getOriginalFilename(),true, 0));

        return productPhotos;
    }

    private List<ProductPhoto> deleteExcessPhotos(int newSize, List<ProductPhoto> productPhotos) {
        if (newSize < productPhotos.size()) {
            for (int i = productPhotos.size() -1; i >= newSize; i--) {
                ProductPhoto photo = productPhotos.get(i);
                s3Service.deleteFile(photo.getPhotoLink());
                Long id = photo.getId();
                photoRepository.deleteByPhotoId(id);
                productPhotos.remove(photo);
            }
        }
        return productPhotos;
    }
    @Override
    public void deleteFiles(List<ProductPhoto> photos){
        for (ProductPhoto photo : photos) {
            s3Service.deleteFile(photo.getPhotoLink());
        }
    }
}
