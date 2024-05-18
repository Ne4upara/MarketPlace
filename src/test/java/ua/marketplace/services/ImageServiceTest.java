//package ua.marketplace.services;
//
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.extension.ExtendWith;
//import org.mockito.InjectMocks;
//import org.mockito.Mock;
//import org.mockito.junit.jupiter.MockitoExtension;
//import org.springframework.http.HttpStatus;
//import org.springframework.web.server.ResponseStatusException;
//import ua.marketplace.entities.Product;
//import ua.marketplace.entities.ProductPhoto;
//import ua.marketplace.repositoryes.PhotoRepository;
//import ua.marketplace.services.impl.ImageService;
//import ua.marketplace.utils.ErrorMessageHandler;
//
//import java.lang.reflect.Method;
//import java.util.ArrayList;
//import java.util.List;
//
//import static org.junit.jupiter.api.Assertions.*;
//import static org.mockito.Mockito.*;
//
//@ExtendWith(MockitoExtension.class)
//@SuppressWarnings("PMD")
//class ImageServiceTest {
//
//    @Mock
//    private PhotoRepository photoRepository;
//
//    @InjectMocks
//    private ImageService imageService;
//
////    @Test
////    void testGetPhotoLinks() {
////        // Given
////        List<String> photos = List.of("photo1.jpg", "photo2.jpg", "photo3.jpg");
////        Product product = new Product();
////
////        // When
////        List<ProductPhoto> result = imageService.getPhotoLinks(photos, product);
////
////        // Then
////        assertNotNull(result);
////        assertEquals(3, result.size());
////        assertEquals(photos.get(0), result.get(0).getPhotoLink());
////        assertTrue(result.get(0).isMainPage());
////        assertFalse(result.get(1).isMainPage());
////        assertFalse(result.get(2).isMainPage());
////    }
//
////    @Test
////    void testGetPhotoLinksWithOver8Img() {
////        // Given
////        List<String> photos = List.of("photo1.jpg", "photo2.jpg", "photo3.jpg", "photo4.jpg",
////                "photo5.jpg", "photo6.jpg", "photo7.jpg", "photo8.jpg", "photo9.jpg");
////        Product product = new Product();
////
////        // When & Then
////        ResponseStatusException exception = assertThrows(ResponseStatusException.class,
////                () -> imageService.getPhotoLinks(photos, product));
////
////        assertEquals(HttpStatus.BAD_REQUEST + " \""
////                        + String.format(ErrorMessageHandler.MAX_LOAD_PHOTO + "\"")
////                , exception.getMessage());
////    }
//
////    @Test
////    void testGetUpdateLinks() {
////        // Given
////        List<String> newPhotoLinks = List.of("new_photo1.jpg", "new_photo2.jpg");
////        Product product = new Product();
////        List<ProductPhoto> existingPhotos = new ArrayList<>();
////        existingPhotos.add(new ProductPhoto());
////        existingPhotos.add(new ProductPhoto());
////        product.setPhotos(existingPhotos);
////
////        // When
////        List<ProductPhoto> result = imageService.getUpdateLinks(newPhotoLinks, product);
////
////        // Then
////        assertNotNull(result);
////        assertEquals(2, result.size());
////        assertEquals(newPhotoLinks.get(0), result.get(0).getPhotoLink());
////        assertFalse(result.get(0).isMainPage());
////        assertEquals(existingPhotos.get(0), result.get(0));
////        assertEquals(newPhotoLinks.get(1), result.get(1).getPhotoLink());
////        assertFalse(result.get(1).isMainPage());
////    }
//
//    @Test
//    void testHandleEmptyNewPhotoLinks_Empty() throws Exception {
//        // Given
//        List<String> newPhotoLinks = List.of();
//        Product product = new Product();
//
//        // When
//        Method method = ImageService.class.getDeclaredMethod("handleEmptyNewPhotoLinks", List.class, Product.class);
//        method.setAccessible(true);
//        List<ProductPhoto> result = (List<ProductPhoto>) method.invoke(imageService, newPhotoLinks, product);
//
//        // Then
//        assertNotNull(result);
//        assertEquals(1, result.size());
//        assertEquals(ErrorMessageHandler.DEFAULT_IMAGE_LINK, result.get(0).getPhotoLink());
//        assertTrue(result.get(0).isMainPage());
//    }
//
//    @Test
//    void testDeleteExcessPhotos() {
//        // Given
//        Product product = new Product();
//        List<ProductPhoto> photos = new ArrayList<>();
//        photos.add(ProductPhoto.builder().id(1L).build());
//        photos.add(ProductPhoto.builder().id(2L).build());
//        photos.add(ProductPhoto.builder().id(3L).build());
//        product.setPhotos(photos);
//
//        // When
//        imageService.deleteExcessPhotos(2, product);
//
//        // Then
//        assertEquals(2, product.getPhotos().size());
//        verify(photoRepository, times(1)).deleteByPhotoId(anyLong());
//    }
//}
