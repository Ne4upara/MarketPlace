package ua.marketplace.services;

import org.springframework.web.multipart.MultipartFile;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.ProductPhoto;

import java.util.List;

/**
 * Service interface for managing images associated with products.
 */
public interface ProductPhotoService {

    /**
     * Retrieves a list of product photos based on the provided photo URLs and product.
     *
     * @param files  The list of photo URLs. //+++++++++++++++++++++++
     * @param product The product associated with the photos.
     * @return A list of product photos.
     */
    List<ProductPhoto> getPhotoLinks(List<MultipartFile> files, Product product);

    /**
     * Retrieves a list of updated product photo links based on the new photo URLs and product.
     *
     * @param files The list of new photo URLs.
     * @param product       The product associated with the photos.
     * @return A list of updated product photos.
     */
    List<ProductPhoto> getUpdateLinks(List<MultipartFile> files, Product product);

    /**
     * Deletes excess photos for the product based on the new size.
     *
     * @param photos The product for which to delete excess photos.
     */
    void deleteFiles(List<ProductPhoto> photos);
}
