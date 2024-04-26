package ua.marketplace.controllers;

import com.fasterxml.jackson.databind.ObjectMapper; // ObjectMapper is used to convert Java objects to JSON strings and vice versa
import org.junit.jupiter.api.Test; // JUnit 5 test annotations
import org.springframework.beans.factory.annotation.Autowired; // Spring Framework annotation to inject dependencies
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc; // Spring Boot annotation to configure MockMvc
import org.springframework.boot.test.context.SpringBootTest; // Spring Boot annotation to run the test within a Spring Boot application context
import org.springframework.context.annotation.Import; // Spring Framework annotation to import additional configuration classes
import org.springframework.http.MediaType; // Spring Framework class to represent HTTP media types
import org.springframework.security.test.context.support.WithMockUser; // Spring Security annotation to simulate an authenticated user
import org.springframework.test.web.servlet.MockMvc; // Spring Framework class to simulate an HTTP client to test Spring MVC controllers
import ua.marketplace.config.TestCacheConfig; // Test configuration class for caching
import ua.marketplace.dto.Pagination; // Data transfer object for pagination
import ua.marketplace.entities.Category; // Entity class for categories
import ua.marketplace.entities.Product; // Entity class for products
import ua.marketplace.entities.ProductPhoto; // Entity class for product photos
import ua.marketplace.mapper.ProductMapper; // Class to map product entities to data transfer objects
import ua.marketplace.repositoryes.ProductRepository; // Repository interface for products
import ua.marketplace.requests.ProductRequest; // Request class for creating or updating a product

import java.math.BigDecimal; // Java class for arbitrary-precision decimal numbers
import java.util.ArrayList; // Java class for a generic list
import java.util.List; // Java interface for a generic list

@SpringBootTest // Run the test within a Spring Boot application context
@AutoConfigureMockMvc // Configure MockMvc for the test
@WithMockUser(username = "test", password = "test") // Simulate an authenticated user for the test
@Import(TestCacheConfig.class) // Import the test configuration class for caching
class ProductControllerTest {

    @Autowired // Inject the MockMvc dependency
    private MockMvc mockMvc;

    @Autowired // Inject the ProductRepository dependency
    private ProductRepository productRepository;

    @Test
    void testGetAllProductsForMainPage() throws Exception {
        // Given, When, Then
        // The test performs a GET request to the endpoint "/v1/products/s/view" with a pagination object as the request body
        // It expects the HTTP status code to be 200 (OK)
        mockMvc.perform(get("/v1/products/s/view")
                        .contentType(MediaType.APPLICATION_JSON) // Set the content type to JSON
                        .content(asJsonString(new Pagination(1, 0L, 1, null)))) // Set the request body to a new Pagination object
                .andExpect(status().isOk()); // Expect the HTTP status code to be 200 (OK)
    }

    @Test
    void testGetAllProductsByCategorySuccessfully() throws Exception {
        // Given, When, Then
        // The test performs a GET request to the endpoint "/v1/products/s/view/dolls" with a pagination object as the request body
        // It expects the HTTP status code to be 200 (OK)
        mockMvc.perform(get("/v1/products/s/view/dolls")
                        .contentType(MediaType.APPLICATION_JSON) // Set the content type to JSON
                        .content(asJsonString(new Pagination(1, 0L, 1, null)))) // Set the request body to a new Pagination object
                .andExpect(status().isOk()); // Expect the HTTP status code to be 200 (OK)
    }

    @Test
    void testGetAllProductsByCategoryWithInvalidCategory() throws Exception {
        // Given, When, Then
        // The test performs a GET request to the endpoint "/v1/products/s/view/invalid_category"
        // It expects the HTTP status code to be 404 (Not Found)
        mockMvc.perform(get("/v1/products/s/view/invalid_category")
                        .contentType(MediaType.APPLICATION_JSON)) // Set the content type to JSON
                .andExpect(status().isNotFound()); // Expect the HTTP status code to be 404 (Not Found)
    }

    @Test
    void testGetProductDetailsById() throws Exception {
        // Given
        // The test creates a new Product object and saves it to the database

        Product product = mockProduct();
        productRepository.save(product);

        // When, Then
        // The test performs a GET request to the endpoint "/v1/products/s/view/details/1"
        // It expects the HTTP status code to be 200 (OK)
        mockMvc.perform(get("/v1/products/s/view/details/1")
                        .contentType(MediaType.APPLICATION_JSON) // Set the content type to JSON
                        .content(String.valueOf(ProductMapper.PRODUCT_INSTANCE.productToProductDto(product)))) // Set the request body to the product object as a JSON string
                .andExpect(status().isOk()); // Expect the HTTP status code to be 200 (OK)

        productRepository.delete(product); // Delete the product object from the database
    }

    @Test
    void testCreateProduct() throws Exception {
        // Given
        // The test creates a new ProductRequest object

        ProductRequest request = mockProductRequest();

        // When, Then
        // The test performs a POST request to the endpoint "/v1/products/create" with the ProductRequest object as the request body
        // It expects the HTTP status code to be 401 (Unauthorized) and the error message to be "User not authorized"
        mockMvc.perform(post("/v1/products/create")
                        .contentType(MediaType.APPLICATION_JSON) // Set the content type to JSON
                        .content(asJsonString(request))) // Set the request body to the ProductRequest object
                .andExpect(status().isUnauthorized()) // Expect the HTTP status code to be 401 (Unauthorized)
                .andExpect(jsonPath("$.errorMessage").value("User not authorized")); // Expect the error message to be "User not authorized"
    }

    @Test
    void testUpdateProduct() throws Exception {
        // Given
        // The test creates a new ProductRequest object and a new Product object
        // It saves the Product object to the database

        ProductRequest request = mockProductRequest();
        Product product = mockProduct();
        productRepository.save(product);

        // When, Then
        // The test performs a PUT request to the endpoint "/v1/products/update/1" with the ProductRequest object as the request body
        // It expects the HTTP status code to be 401 (Unauthorized) and the error message to be "User not authorized"
        mockMvc.perform(put("/v1/products/update/1")
                        .contentType(MediaType.APPLICATION_JSON) // Set the content type to JSON
                        .content(asJsonString(request))) // Set the request body to the ProductRequest object
                .andExpect(status().isUnauthorized()) // Expect the HTTP status code to be 401 (Unauthorized)
                .andExpect(jsonPath("$.errorMessage").value("User not authorized")); // Expect the error message to be "User not authorized"

        productRepository.delete(product); // Delete the Product object from the database
    }

    @Test
    void testDeleteProduct() throws Exception {
        // Given
        // The test creates a new ProductRequest object and a new Product object
        // It saves the Product object to the database

        ProductRequest request = mockProductRequest();
        Product product = mockProduct();
        productRepository.save(product);

        // When, Then
        // The test performs a DELETE request to the endpoint "/v1/products/delete/1" with the ProductRequest object as the request body
        // It expects the HTTP status code to be 401 (Unauthorized) and the error message to be "User not authorized"
        mockMvc.perform(delete("/v1/products/delete/1")
                        .contentType(MediaType.APPLICATION_JSON) // Set the content type to JSON
                        .content(asJsonString(request))) // Set the request body to the ProductRequest object
                .andExpect(status().isUnauthorized()) // Expect the HTTP status code to be 401 (Unauthorized)
                .andExpect(jsonPath("$.errorMessage").value("User not authorized")); // Expect the error message to be "User not authorized"

    }

    private Product mockProduct() {
        // Create a new Product object with a list of ProductPhoto objects, a Category object, and set other fields
        List<ProductPhoto> photo = new ArrayList<>();
        Category category = new Category(1L, "Test", "ТЕСТ");
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

    private ProductRequest mockProductRequest() {
        // Create a new ProductRequest object with all fields set
        return new ProductRequest
                ("Test Product",
                        new ArrayList<>(),
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
        // Convert the Java object to a JSON string using ObjectMapper
        try {
            return new ObjectMapper().writeValueAsString(obj);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

}
