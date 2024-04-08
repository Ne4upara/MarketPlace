package ua.marketplace.controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;
import ua.marketplace.data.Category;
import ua.marketplace.dto.Pagination;
import ua.marketplace.entities.Product;
import ua.marketplace.entities.ProductPhoto;
import ua.marketplace.mapper.ProductMapper;
import ua.marketplace.repositoryes.ProductRepository;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@SpringBootTest
@AutoConfigureMockMvc
@WithMockUser(username = "test", password = "test")
class ProductControllerTest {

    @Autowired
    private MockMvc mockMvc;
    @Autowired
    private ProductRepository productRepository;

    @Test
    void testGetAllProductsForMainPage() throws Exception {

        //Given,When,Then
        mockMvc.perform(get("/api/v1/products/s/list")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(new Pagination(1, 0L, 1, null))))
                .andExpect(status().isOk());
    }

    @Test
    void testGetAllProductsByCategorySuccessfully() throws Exception {

        //Given,When,Then
        mockMvc.perform(get("/api/v1/products/s/list/dolls")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(new Pagination(1, 0L, 1, null))))
                .andExpect(status().isOk());
    }

    @Test
    void testGetAllProductsByCategoryWithInvalidCategory() throws Exception {

        //Given,When,Then
        mockMvc.perform(get("/api/v1/products/s/list/invalid_category")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isConflict());
    }


    @Test
    void testGetProductDetailsById() throws Exception {
        //Given
        Product product = mockProduct();
        productRepository.save(product);

        //When,Then
        mockMvc.perform(get("/api/v1/products/s/details/1")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(String.valueOf(ProductMapper.PRODUCT_INSTANCE.productToProductDto(product))))
                .andExpect(status().isOk());

        productRepository.delete(product);
    }

//    @Test
//    void testCreateProduct() throws Exception {
//        //Given
//        ProductRequest request = mockProductRequest();
//
//        //When,Then
//        mockMvc.perform(post("/api/v1/products/create")
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(asJsonString(request)))
//                .andExpect(status().isUnauthorized())
//                .andExpect(jsonPath("$.errorMessage").value("User not authorized"));
//
//    }

//    @Test
//    void testUpdateProduct() throws Exception {
//        //Given
//        ProductRequest request = mockProductRequest();
//        Product product = mockProduct();
//        productRepository.save(product);
//
//        //When,Then
//        mockMvc.perform(put("/api/v1/products/update/1")
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(asJsonString(request)))
//                .andExpect(status().isUnauthorized())
//                .andExpect(jsonPath("$.errorMessage").value("User not authorized"));
//
//        productRepository.delete(product);
//    }

//    @Test
//    void testRateProduct() throws Exception {
//        //Given
//        ProductRequest request = mockProductRequest();
//        Product product = mockProduct();
//        productRepository.save(product);
//
//        //When,Then
//        mockMvc.perform(patch("/api/v1/products/1/rate/1")
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(asJsonString(request)))
//                .andExpect(status().isUnauthorized())
//                .andExpect(jsonPath("$.errorMessage").value("User not authorized"));
//
//        productRepository.delete(product);
//    }

//    @Test
//    void testDeleteProduct() throws Exception {
//        //Given
//        ProductRequest request = mockProductRequest();
//        Product product = mockProduct();
//        productRepository.save(product);
//
//        //When,Then
//        mockMvc.perform(delete("/api/v1/products/delete/1")
//                        .contentType(MediaType.APPLICATION_JSON)
//                        .content(asJsonString(request)))
//                .andExpect(status().isUnauthorized())
//                .andExpect(jsonPath("$.errorMessage").value("User not authorized"));
//
//    }

    private Product mockProduct() {
        List<ProductPhoto> photo = new ArrayList<>();
        Category category = new Category(1L, "Test");
        return Product
                .builder()
                .productName("test")
                .photos(photo)
//                .productPhotoLink("Test photo link")
                .productPrice(BigDecimal.valueOf(10))
                .productDescription("test description")
                .category(category)
                .productType("new")
                .productQuantity(10)
                .owner(null)
                .build();
    }

//    private ProductRequest mockProductRequest() {
//        return new ProductRequest
//                ("Test Product",
//                        null,
//                        BigDecimal.valueOf(10.10),
//                        "Опис товару",
//                        ProductCategory.BOARD_GAMES,
//                        "new",
//                        10);
//    }

    private String asJsonString(Object obj) {
        try {
            return new ObjectMapper().writeValueAsString(obj);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
