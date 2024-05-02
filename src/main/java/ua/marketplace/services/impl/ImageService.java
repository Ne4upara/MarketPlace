package ua.marketplace.services.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.ProductPhoto;
import ua.marketplace.repositoryes.PhotoRepository;
import ua.marketplace.services.IImageService;
import ua.marketplace.utils.ErrorMessageHandler;

import java.util.ArrayList;
import java.util.List;

/**
 * Implementation of the {@link IImageService} interface for managing images associated with products.
 */
@Service
@RequiredArgsConstructor
public class ImageService implements IImageService {

    private final PhotoRepository photoRepository;
    private static final int MAX_PHOTOS_ALLOWED = 8;

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
}
