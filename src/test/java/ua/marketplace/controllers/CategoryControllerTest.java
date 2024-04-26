package ua.marketplace.controllers; // The controller is part of this package

import org.junit.jupiter.api.Test; // JUnit testing framework
import org.springframework.beans.factory.annotation.Autowired; // Dependency injection
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc; // MockMvc configuration
import org.springframework.boot.test.context.SpringBootTest; // Spring Boot testing framework
import org.springframework.http.MediaType; // HTTP media types
import org.springframework.test.web.servlet.MockMvc; // Mock HTTP requests

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get; // GET request builder
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content; // Content result matcher
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status; // Status result matcher

@SpringBootTest // Indicates that this is a Spring Boot test
@AutoConfigureMockMvc // Configures MockMvc for this test
@SuppressWarnings("PMD") // Suppresses PMD warnings
class CategoryControllerTest { // Test class for CategoryController

    @Autowired // Injects MockMvc instance
    private MockMvc mockMvc; // MockMvc instance for testing

    @Test // Indicates that this is a test method
    void testGetAllCategory() throws Exception { // Test method for GET /v1/categories/list

        // Given, When, Then
        mockMvc.perform(get("/v1/categories/list") // Send a GET request to /v1/categories/list
                        .contentType(MediaType.APPLICATION_JSON)) // Set the content type to JSON
                .andExpect(status().isOk()) // Expect an OK status (200)
                .andExpect(content().contentType(MediaType.APPLICATION_JSON)); // Expect the content type to be JSON
    }

}

