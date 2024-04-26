package ua.marketplace.services;

import ua.marketplace.entities.Product;
import ua.marketplace.entities.ProductPhoto;

import java.util.List;

/**
 * Service interface for managing images associated with products.
 */
public interface IImageService {

    /**
     * Retrieves a list of product photos based on the provided photo URLs and product.
     *
     * @param photos  The list of photo URLs.
     * @param product The product associated with the photos.
     * @return A list of product photos.
     */
    List<ProductPhoto> getPhotoLinks(List<String> photos, Product product);

    /**
     * Retrieves a list of updated product photo links based on the new photo URLs and product.
     *
     * @param newPhotoLinks The list of new photo URLs.
     * @param product       The product associated with the photos.
     * @return A list of updated product photos.
     */
    List<ProductPhoto> getUpdateLinks(List<String> newPhotoLinks, Product product);

    /**
     * Deletes excess photos for the product based on the new size.
     *
     * @param newSize The new size of the photos.
     * @param product The product for which to delete excess photos.
     */
    void deleteExcessPhotos(int newSize, Product product);

}
