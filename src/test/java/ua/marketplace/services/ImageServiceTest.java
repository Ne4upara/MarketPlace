package ua.marketplace.services;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.ProductPhoto;
import ua.marketplace.repositoryes.PhotoRepository;
import ua.marketplace.utils.ErrorMessageHandler;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

// The test class for ImageService
@ExtendWith(MockitoExtension.class)
@SuppressWarnings("PMD")
class ImageServiceTest {

    // Mock the PhotoRepository
    @Mock
    private PhotoRepository photoRepository;

    // Inject the mocked PhotoRepository into ImageService
    @InjectMocks
    private ImageService imageService;

    // Test method for getPhotoLinks
    @Test
    void testGetPhotoLinks() {
        // Given
        List<String> photos = List.of("photo1.jpg", "photo2.jpg", "photo3.jpg");
        Product product = new Product();

        // When
        List<ProductPhoto> result = imageService.getPhotoLinks(photos, product);

        // Then
        // Assert that the result is not null and has the correct size
        assertNotNull(result);
        assertEquals(3, result.size());

        // Assert that the first ProductPhoto has the correct photoLink and isMainPage value
        assertEquals(photos.get(0), result.get(0).getPhotoLink());
        assertTrue(result.get(0).isMainPage());

        // Assert that the second and third ProductPhotos have the correct isMainPage value
        assertFalse(result.get(1).isMainPage());
        assertFalse(result.get(2).isMainPage());
    }

    // Test method for getPhotoLinks with more than 8 images
    @Test
    void testGetPhotoLinksWithOver8Img() {
        // Given
        List<String> photos = List.of("photo1.jpg", "photo2.jpg", "photo3.jpg", "photo4.jpg",
                "photo5.jpg", "photo6.jpg", "photo7.jpg", "photo8.jpg", "photo9.jpg");
        Product product = new Product();

        // When & Then
        // Assert that a ResponseStatusException is thrown with the correct error message
        ResponseStatusException exception = assertThrows(ResponseStatusException.class, () -> {
            imageService.getPhotoLinks(photos, product);
        });

        assertEquals(HttpStatus.BAD_REQUEST + " \""
                        + String.format(ErrorMessageHandler.MAX_LOAD_PHOTO + "\"")
                , exception.getMessage());
    }

    // Test method for getUpdateLinks
    @Test
    void testGetUpdateLinks() {
        // Given
        List<String> newPhotoLinks = List.of("new_photo1.jpg", "new_photo2.jpg");
        Product product = new Product();
        List<ProductPhoto> existingPhotos = new ArrayList<>();
        existingPhotos.add(new ProductPhoto());
        existingPhotos.add(new ProductPhoto());
        product.setPhotos(existingPhotos);

        // When
        List<ProductPhoto> result = imageService.getUpdateLinks(newPhotoLinks, product);

        // Then
        // Assert that the result is not null and has the correct size
        assertNotNull(result);
        assertEquals(2, result.size());

        // Assert that the first ProductPhoto has the correct photoLink and isMainPage value
        assertEquals(newPhotoLinks.get(0), result.get(0).getPhotoLink());
        assertFalse(result.get(0).isMainPage());

        // Assert that the second ProductPhoto has the correct photoLink and isMainPage value
        assertEquals(existingPhotos.get(0), result.get(0));
        assertEquals(newPhotoLinks.get(1), result.get(1).getPhotoLink());
        assertFalse(result.get(1).isMainPage());
    }

    // Test method for handleEmptyNewPhotoLinks
    @Test
    void testHandleEmptyNewPhotoLinks_Empty() throws Exception {
        // Given
        List<String> newPhotoLinks = List.of();
        Product product = new Product();

        // When
        // Use reflection to call the private handleEmptyNewPhotoLinks method
        Method method = ImageService.class.getDeclaredMethod("handleEmptyNewPhotoLinks", List.class, Product.class);
        method.setAccessible(true);
        List<ProductPhoto> result = (List<ProductPhoto>) method.invoke(imageService, newPhotoLinks, product);

        // Then
        // Assert that the result is not null and has the correct size
        assertNotNull(result);
        assertEquals(1, result.size());

        // Assert that the ProductPhoto has the correct photoLink and isMainPage value
        assertEquals(ErrorMessageHandler.DEFAULT_IMAGE_LINK, result.get(0).getPhotoLink());
        assertTrue(result.get(0).isMainPage());
    }

    // Test method for deleteExcessPhotos
    @Test
    void testDeleteExcessPhotos() {
        // Given
        Product product = new Product();
        List<ProductPhoto> photos = new ArrayList<>();
        photos.add(ProductPhoto.builder().id(1L).build());
        photos.add(ProductPhoto.builder().id(2L).build());
        photos.add(ProductPhoto.builder().id(3L).build());
        product.setPhotos(photos);

        // When
        imageService.deleteExcessPhotos(2, product);

        // Then
        // Assert that the product has the correct number of ProductPhotos
        assertEquals(2, product.getPhotos().size());

        // Assert that the PhotoRepository's deleteByPhotoId method was called once with the correct argument
        verify(photoRepository, times(1)).deleteByPhotoId(anyLong());
    }
}
