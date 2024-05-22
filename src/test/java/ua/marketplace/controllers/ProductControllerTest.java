package ua.marketplace.controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.annotation.Rollback;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.transaction.annotation.Transactional;
import ua.marketplace.BaseTest;
import ua.marketplace.config.TestCacheConfig;
import ua.marketplace.dto.Pagination;
import ua.marketplace.entities.Category;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.ProductPhoto;
import ua.marketplace.entities.User;
import ua.marketplace.mapper.ProductMapper;
import ua.marketplace.repositoryes.ProductRepository;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.ProductRequest;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


@Transactional
//@WithMockUser(username = "testuser")
class ProductControllerTest extends BaseTest {

    @Autowired
    private MockMvc mockMvc;
    @Autowired
    private ProductRepository productRepository;
    @Autowired
    private UserRepository userRepository;

    @Test
    @Rollback
    void testGetAllProductsForMainPage() throws Exception {

        //Given,When,Then
        mockMvc.perform(get("/v1/products/s/view")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(new Pagination(1, 0L, 1, null))))
                .andExpect(status().isOk());
    }

    @Test
    @Rollback
    void testGetAllProductsByCategorySuccessfully() throws Exception {

        //Given,When,Then
        mockMvc.perform(get("/v1/products/s/view/dolls")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(new Pagination(1, 0L, 1, null))))
                .andExpect(status().isOk());
    }

    @Test
    @Rollback
    void testGetAllProductsByCategoryWithInvalidCategory() throws Exception {

        //Given,When,Then
        mockMvc.perform(get("/v1/products/s/view/invalid_category")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound());
    }


    @Test
    @Rollback
    void testGetProductDetailsById() throws Exception {
        //Given
        Product product = mockProduct();
        Product saved = productRepository.save(product);

        //When,Then
        mockMvc.perform(get("/v1/products/s/view/details/"+saved.getId())
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(String.valueOf(ProductMapper.PRODUCT_INSTANCE.productToProductDto(product))))
                .andExpect(status().isOk());
    }

    @Test
    @Rollback
    void testCreateProduct() throws Exception {
        // Given
        ProductRequest request = mockProductRequest();
        String jsonRequest = asJsonString(request);

        // Create a mock file
        MockMultipartFile mockFile = mockFile();

        // Create a mock JSON request part
        MockMultipartFile jsonPart = new MockMultipartFile(
                "request",
                "",
                MediaType.APPLICATION_JSON_VALUE,
                jsonRequest.getBytes()
        );

        // When, Then
        mockMvc.perform(multipart("/v1/products/create")
                        .file(mockFile)
                        .file(jsonPart)
                        .contentType(MediaType.MULTIPART_FORM_DATA)
                )
                .andExpect(status().isCreated());
//                .andExpect(jsonPath("$.errorMessage").value("User not authorized"));
    }

    @Test
    @Rollback
    void testUpdateProduct() throws Exception {
        // Given
        ProductRequest request = mockProductRequest();
        Product product = mockProduct();

        Optional<User> test = userRepository.findByPhoneNumber("test");
        product.setOwner(test.get());
        productRepository.save(product);

        // Create a mock file
        MockMultipartFile mockFile = mockFile();

        // Create a mock JSON request part
        MockMultipartFile jsonPart = new MockMultipartFile(
                "request",
                "",
                MediaType.APPLICATION_JSON_VALUE,
                asJsonString(request).getBytes()
        );

        // When, Then
        mockMvc.perform(multipart("/v1/products/update/" + product.getId())
                        .file(mockFile)
                        .file(jsonPart)
                        .contentType(MediaType.MULTIPART_FORM_DATA)
                        .with(req -> {
                            req.setMethod("PUT");
                            return req;
                        })
                )
                .andExpect(status().isOk());
//                .andExpect(jsonPath("$.errorMessage").value("User not authorized"));

        productRepository.delete(product);
    }

    @Test
    @Rollback
    void testDeleteProduct() throws Exception {
        //Given
        ProductRequest request = mockProductRequest();
        Product product = mockProduct();
        productRepository.save(product);

        //When,Then
        mockMvc.perform(delete("/v1/products/delete/1")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isNoContent());
//                .andExpect(jsonPath("$.errorMessage").value("User not authorized"));

    }

    @Test
    @Rollback
    void testAddFavoriteProduct() throws Exception {

        ProductRequest request = mockProductRequest();
        Product product = mockProduct();
        productRepository.save(product);

        mockMvc.perform(post("/v1/products/favorite/1")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isOk());
//                .andExpect(jsonPath("$.errorMessage").value("User not authorized"));
    }

    @Test
    @Rollback
    void testRemoveFavoriteProduct() throws Exception {

        ProductRequest request = mockProductRequest();

        mockMvc.perform(delete("/v1/products/favorite/1")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isOk());
//                .andExpect(jsonPath("$.errorMessage").value("User not authorized"));
    }

    private Product mockProduct() {
        List<ProductPhoto> photo = new ArrayList<>();
        Category category = new Category(1L, "Test", "ТЕСТ");
        return Product
                .builder()
                .id(1L)
                .productName("test")
                .photos(photo)
                .productPrice(BigDecimal.valueOf(10))
                .productDescription("test description")
                .category(category)
                .productType("new")
                .owner(null)
                .build();
    }

    private ProductRequest mockProductRequest() {
        return new ProductRequest
                ("Test Product",
                        BigDecimal.valueOf(10),
                        "test description",
                        "dolls",
                        "new",
                        "Seller",
                        "+380999999999",
                        "seller@mail.com",
                        "Location"
                );
    }

    private String asJsonString(Object obj) {
        try {
            return new ObjectMapper().writeValueAsString(obj);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private MockMultipartFile mockFile() {
        return new MockMultipartFile(
                "files",
                "test.txt",
                MediaType.TEXT_PLAIN_VALUE,
                "This is a test file content".getBytes());
    }
}