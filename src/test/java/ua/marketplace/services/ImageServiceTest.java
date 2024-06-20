package ua.marketplace.services;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.function.Executable;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.server.ResponseStatusException;
import ua.marketplace.entities.Category;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.ProductPhoto;
import ua.marketplace.repositoryes.PhotoRepository;
import ua.marketplace.services.impl.ProductPhotoServiceImpl;
import ua.marketplace.services.impl.S3ServiceImpl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;
import static ua.marketplace.utils.ErrorMessageHandler.INCORRECT_FILE_FORMAT;

@ExtendWith(MockitoExtension.class)
class ImageServiceTest {

    @Mock
    private PhotoRepository photoRepository;
    
    @Mock
    private S3ServiceImpl s3Service;
    
    @InjectMocks
    private ProductPhotoServiceImpl imageService;

    private Product product;
    private MultipartFile file1;
    private MultipartFile file2;
    private List<MultipartFile> files;

    @BeforeEach
    void setUp() {
        product = mockProduct();
        product.setPhotos(new ArrayList<>());

        file1 = mock(MultipartFile.class);
        file2 = mock(MultipartFile.class);

        files = new ArrayList<>();
        files.add(file1);
        files.add(file2);
    }

    @Test
    void testGetPhotoLinksSuccess() {
        // Given
        when(file1.getOriginalFilename()).thenReturn("file1.jpg");
        when(file2.getOriginalFilename()).thenReturn("file2.jpg");
        when(s3Service.uploadFile(file1)).thenReturn("url1");
        when(s3Service.uploadFile(file2)).thenReturn("url2");

        // When
        List<ProductPhoto> result = imageService.getPhotoLinks(files, mockProduct());

        // Then
        assertEquals(3, result.size());
        assertEquals("url1", result.get(1).getPhotoLink());
        assertEquals("url2", result.get(2).getPhotoLink());
    }

    @Test
    void testGetPhotoLinksWithInvalidFormat() {
        // Given
        when(file1.getOriginalFilename()).thenReturn("file1.txt");

        // When
        Executable executable = () -> imageService.getPhotoLinks(files, product);

        // Then
        ResponseStatusException exception = assertThrows(ResponseStatusException.class, executable);
        assertEquals(HttpStatus.BAD_REQUEST, exception.getStatusCode());
        assertEquals(String.format(INCORRECT_FILE_FORMAT, "file1.txt"), exception.getReason());
    }

    @Test
    void testGetPhotoLinks_DefaultImageLink() {
        // Given
        lenient().when(file1.getOriginalFilename()).thenReturn("");
        lenient().when(file2.getOriginalFilename()).thenReturn("file2.jpg");
        lenient().when(s3Service.uploadFile(file2)).thenReturn("url2");

        // When
        List<ProductPhoto> result = imageService.getPhotoLinks(files, mockProduct());

        // Then
        assertEquals(3, result.size());
        assertEquals(ProductPhotoServiceImpl.DEFAULT_IMAGE_LINK, result.get(1).getPhotoLink());
        assertEquals("url2", result.get(2).getPhotoLink());
    }

    @Test
    void testGetUpdateLinks_AddNewPhoto() {
        // Given
        ProductPhoto existingPhoto = new ProductPhoto();
        existingPhoto.setMainPage(false);
        existingPhoto.setNumberPhoto(1);
        existingPhoto.setOriginalName("existing.jpg");
        existingPhoto.setPhotoLink("existing_url");

        product.getPhotos().add(existingPhoto);

        when(file1.getOriginalFilename()).thenReturn("newFile.jpg");

        // When
        List<ProductPhoto> result = imageService.getUpdateLinks(files, product);

        // Then
        assertEquals(2, result.size());
    }


    @Test
    void testValidateMaxLink_ThrowsException() {
        // Given
        List<MultipartFile> tooManyFiles = new ArrayList<>();
        for (int i = 0; i < 9; i++) {
            tooManyFiles.add(mock(MultipartFile.class));
        }

        // When & Then
        assertThrows(ResponseStatusException.class, () -> imageService.getPhotoLinks(tooManyFiles, product));
    }

    private Product mockProduct() {
        List<ProductPhoto> photo = new ArrayList<>();
        Category category = new Category(1L, "dolls", "Ляльки, Пупси");
        return Product
                .builder()
                .productName("test")
                .photos(photo)
                .productPrice(BigDecimal.valueOf(10))
                .productDescription("test description")
                .category(category)
                .productType("new")
                .owner(null)
                .build();
    }
}
