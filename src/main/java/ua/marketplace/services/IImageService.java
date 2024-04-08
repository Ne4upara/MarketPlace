package ua.marketplace.services;

import ua.marketplace.entities.Product;
import ua.marketplace.entities.ProductPhoto;

import java.util.List;

public interface IImageService {

    List<ProductPhoto> getPhotoLinks(List<String> photos, Product product);

    List<ProductPhoto> getUpdateLinks(List<String> newPhotoLinks, Product product);

    void deleteExcessPhotos(int newSize, Product product);
}
