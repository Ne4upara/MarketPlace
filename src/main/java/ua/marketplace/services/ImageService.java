package ua.marketplace.services;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.ProductPhoto;
import ua.marketplace.repositoryes.PhotoRepository;
import ua.marketplace.utils.ErrorMessageHandler;

/**
 * This class, ImageService, implements the IImageService interface for managing images associated with products.
 * It provides methods for retrieving, updating, and deleting product photos.
 */
@Service
@RequiredArgsConstructor
public class ImageService implements IImageService {

    // Dependency injection of the PhotoRepository
    private final PhotoRepository photoRepository;

    // Constant for the maximum number of photos allowed per product
    private static final int MAX_PHOTOS_ALLOWED = 8;

    /**
     * This method retrieves a list of product photos based on the provided photo URLs and product.
     * It handles empty new photo links and creates new ProductPhoto instances for each URL.
     *
     * @param photos  The list of photo URLs.
     * @param product The product associated with the photos.
     * @return A list of product photos.
     */
    @Override
    public List<ProductPhoto> getPhotoLinks(List<String> photos, Product product) {
        List<ProductPhoto> productPhotos = handleEmptyNewPhotoLinks(photos, product);

        // Iterate through the photo URLs and create ProductPhoto instances
        for (int i = 0; i < photos.size(); i++) {
            String photoLink = photos.get(i);
            ProductPhoto productPhoto = createProductPhoto(photoLink, product, i == 0);
            productPhotos.add(productPhoto);
        }
        return productPhotos;
    }

    // Helper method for creating a new ProductPhoto instance
    private ProductPhoto createProductPhoto(String photoLink, Product product, boolean isMainPage) {
        return ProductPhoto.builder()
                .photoLink(photoLink)
                .product(product)
                .mainPage(isMainPage)
                .build();
    }

    // Helper method for handling empty new photo links and setting the default image link
    private void isMaxLink(int size) {
        if (size > MAX_PHOTOS_ALLOWED) {
            throw new ResponseStatusException
                    (HttpStatus.BAD_REQUEST, String.format(ErrorMessageHandler.MAX_LOAD_PHOTO));
        }
    }

    /**
     * This method retrieves a list of updated product photo links based on the new photo URLs and product.
     * It handles empty new photo links and updates or creates new ProductPhoto instances for each URL.
     *
     * @param newPhotoLinks The list of new photo URLs.
     * @param product       The product associated with the photos.
     * @return A list of updated product photos.
     */
    @Override
    public List<ProductPhoto> getUpdateLinks(List<String> newPhotoLinks, Product product) {
        List<ProductPhoto> productPhotos = handleEmptyNewPhotoLinks(newPhotoLinks, product);

        // Iterate through the new photo URLs and update or create ProductPhoto instances
        for (int i = 0; i < newPhotoLinks.size(); i++) {
            String photoLink = newPhotoLinks.get(i);
            if (i < product.getPhotos().size()) {
                // Update existing ProductPhoto instances
                ProductPhoto existingPhoto = product.getPhotos().get(i);
                existingPhoto.setPhotoLink(photoLink);
                productPhotos.add(existingPhoto);
            } else {
                // Create new ProductPhoto instances
                ProductPhoto newPhoto = createProductPhoto(photoLink, product, i == 0);
                productPhotos.add(newPhoto);
            }
        }

        return productPhotos;
    }

    // Helper method for handling empty new photo links and setting the default image link
    private List<ProductPhoto> handleEmptyNewPhotoLinks(List<String> newPhotoLinks, Product product) {
        List<ProductPhoto> productPhotos = new ArrayList<>();

        // Check if the new photo links are empty or exceed the maximum number of photos allowed
        int newSize = newPhotoLinks.size();
        isMaxLink(newSize);
        if (newSize == 0) {
            // Set the default image link if there are no new photo links
            productPhotos.add(createProductPhoto(ErrorMessageHandler.DEFAULT_IMAGE_LINK, product, true));
            return productPhotos;
        }
        return productPhotos;
    }

    /**
     * This method deletes excess photos for the product based on the new size.
     *
     * @param newSize The new size of the photos.
     * @param product The product for which to delete excess photos.
     */
    @Override
    public void deleteExcessPhotos(int newSize, Product product) {
        int currentSize = product.getPhotos().size();

        // Check if there are excess photos and delete them
        if (newSize < currentSize) {
            List<ProductPhoto> listPhoto = product.getPhotos();
            for (int i = currentSize - 1; i >= newSize; i--) {
                ProductPhoto photo = listPhoto.get(i);
                Long id = photo.getId();
                photoRepository.deleteByPhotoId(id);
                listPhoto.remove(photo);
            }
        }
    }
}
