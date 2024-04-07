package ua.marketplace.services;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.ProductPhoto;
import ua.marketplace.repositoryes.PhotoRepository;
import ua.marketplace.repositoryes.ProductRepository;
import ua.marketplace.utils.ErrorMessageHandler;

import java.util.ArrayList;
import java.util.List;

@Service
@RequiredArgsConstructor
public class ImageService {

    private final PhotoRepository photoRepository;
    private final ProductRepository productRepository;

    public List<ProductPhoto> getPhotoLinks(List<String> photos, Product product){
        List<ProductPhoto> productPhotos = new ArrayList<>();
        int size = photos.size();
        isMaxLink(size);
        if (size == 0){
            productPhotos.add(createDefaultProductPhoto(product));
            return productPhotos;
        }

        for (int i = 0; i < size; i++) {
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

    private ProductPhoto createDefaultProductPhoto(Product product){
            return ProductPhoto.builder()
                    .photoLink("https://i.pinimg.com/1200x/9f/ab/e5/9fabe5f90ca53f9a86306203f517f9fd.jpg")
                    .product(product)
                    .mainPage(true)
                    .build();
    }

    private void isMaxLink(int size){
        if (size > 8) {
            throw new ResponseStatusException
                    (HttpStatus.BAD_REQUEST, String.format(ErrorMessageHandler.MAX_LOAD_PHOTO));
        }
    }

    public List<ProductPhoto> getUpdateLinks(List<String> newPhotoLinks, Product product) {
        List<ProductPhoto> productPhotos = new ArrayList<>();
        int newSize = newPhotoLinks.size();
        isMaxLink(newSize);
//        productPhotos = deleteExcessPhotos(newSize, product);
        if (newSize == 0){

            productPhotos.add(createDefaultProductPhoto(product));
            return productPhotos;
        }


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
//        productRepository.save(product);
        return productPhotos;
    }

    public List<ProductPhoto> deleteExcessPhotos(int newSize, Product product) {
//        int currentSize = product.getPhotos().size();
//        List<ProductPhoto> listPhoto = product.getPhotos();
//        if (newSize < currentSize) {
//
//            for (int i = currentSize; i > newSize; i--) {
//                ProductPhoto photo = listPhoto.get(i-1);
//                photoRepository.delete(photo);
//            }
//        }
//        return listPhoto;
//        int currentSize = product.getPhotos().size();
//        if (newSize < currentSize) {
//            List<ProductPhoto> photosToRemove = product.getPhotos().subList(newSize, currentSize);
//            List<Long> photoIdsToRemove = photosToRemove.stream()
//                    .map(ProductPhoto::getId)
//                    .toList();
//            photoRepository.deleteAllById(photoIdsToRemove);
//        }
//        int currentSize = product.getPhotos().size();
//        if (newSize < currentSize) {
//            List<ProductPhoto> photosToRemove = product.getPhotos().subList(newSize, currentSize);
//            photoRepository.deleteAll(photosToRemove);
//        }
        int currentSize = product.getPhotos().size();
        List<ProductPhoto> listPhoto = product.getPhotos();
        if (newSize < currentSize) {

            for (int i = currentSize - 1; i >= newSize; i--) {
                ProductPhoto photo = listPhoto.get(i);
                Long id = photo.getId();
                photoRepository.deleteByPhotoId(id);
//                photoRepository.delete(photo);
                listPhoto.remove(photo); // Удаление фотографии из списка продукта и
//                если его закоментировать записи остаються в БД
            }
        }
//        listPhoto = product.getPhotos().subList(newSize, currentSize);
        return listPhoto;
//    }
//        List<ProductPhoto> photos = product.getPhotos();
//        for (int i = 0; i < 10; i++){
//            ProductPhoto photo = photos.get(i);
//            Long id = photo.getId();
//
//        }


    }
}