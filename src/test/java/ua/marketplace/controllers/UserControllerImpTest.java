package ua.marketplace.controllers;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;
import ua.marketplace.config.TestCacheConfig;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


@SpringBootTest
@AutoConfigureMockMvc
@WithMockUser(username = "test", password = "test")
@Import(TestCacheConfig.class)
class UserControllerImpTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    void testGetViewMyProduct() throws Exception {

        mockMvc.perform(get("/v1/my-profile/view/all")
                        .param("number", "0")
                        .param("size", "10")
                        .param("sort", "creationDate")
                        .param("order", "ASC"))
                .andExpect(status().isUnauthorized());
    }

    @Test
    void testGetUserInfo() throws Exception {
        mockMvc.perform(get("/v1/my-profile/info"))
                .andExpect(status().isOk());
    }

    @Test
    void testGetAllFavorite() throws Exception {
        mockMvc.perform(get("/v1/my-profile/favorite/all")
                        .param("number", "0")
                        .param("size", "10")
                        .param("sort", "creationDate")
                        .param("order", "ASC"))
                .andExpect(status().isUnauthorized());
    }
}
