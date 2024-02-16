package ua.marketplace.controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import ua.marketplace.entities.SmsCode;
import ua.marketplace.entities.User;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.CheckCodeRequest;
import ua.marketplace.requests.LoginRequest;
import ua.marketplace.requests.RegistrationRequest;

import java.time.LocalDateTime;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * Unit tests for {@link PhoneAuthController}.
 */
@SpringBootTest
@AutoConfigureMockMvc
class PhoneAuthControllerTest {

    @Autowired
    private MockMvc mockMvc;
    @Autowired
    private UserRepository userRepository;

    /**
     * Test for successful registration.
     *
     * @throws Exception if any error occurs during the test
     */
    @Test
    void testRegistrationSuccessfully() throws Exception {

        //Given
        RegistrationRequest request = RegistrationRequest
                .builder()
                .firstName("test")
                .phoneNumber("+38(099)999-99-99")
                .build();

        //When,Then
        mockMvc.perform(post("/api/v1/auth/phone/registration")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.success").value(true));
    }

    /**
     * Test for registration with invalid request.
     *
     * @throws Exception if any error occurs during the test
     */
    @Test
    void testRegistrationWithInvalidRequest() throws Exception {

        //Given
        RegistrationRequest request = RegistrationRequest
                .builder()
                .firstName("")
                .phoneNumber("")
                .build();

        //When,Then
        mockMvc.perform(post("/api/v1/auth/phone/registration")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.success").value(false));
    }

    /**
     * Test for successful login.
     *
     * @throws Exception if any error occurs during the test
     */
    @Test
    void testLoginSuccessfully() throws Exception {

        //Given
        User user = User
                .builder()
                .firstName("test")
                .phoneNumber("+38(099)999-99-91")
                .build();
        userRepository.save(user);

        LoginRequest request = LoginRequest
                .builder()
                .phoneNumber("+38(099)999-99-91")
                .build();

        //When,Then
        mockMvc.perform(post("/api/v1/auth/phone/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.success").value(true));

        userRepository.delete(user);
    }

    /**
     * Test for login with invalid request.
     *
     * @throws Exception if any error occurs during the test
     */
    @Test
    void testLoginWithInvalidRequest() throws Exception {

        //Given
        LoginRequest request = LoginRequest
                .builder()
                .phoneNumber("")
                .build();

        //When,Then
        mockMvc.perform(post("/api/v1/auth/phone/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.success").value(false));
    }

    /**
     * Test for successfully checking code.
     *
     * @throws Exception if any error occurs during the test
     */
    @Test
    void testCheckCodeSuccessfully() throws Exception {

        CheckCodeRequest request = CheckCodeRequest
                .builder()
                .smsCode("1111")
                .build();

        //When,Then
        mockMvc.perform(post("/api/v1/auth/phone/login/code")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.success").value(false));

    }

    /**
     * Test for checking code with invalid data.
     *
     * @throws Exception if any error occurs during the test
     */
    @Test
    void testCheckCodeWithInvalidData() throws Exception {

        //Given
        User user = User
                .builder()
                .firstName("test")
                .phoneNumber("+38(099)999-99-99")
                .smsCode(SmsCode
                        .builder()
                        .code("1111")
                        .createAt(LocalDateTime.now())
                        .build())
                .build();
        userRepository.save(user);

        CheckCodeRequest request = CheckCodeRequest
                .builder()
                .smsCode("")
                .build();


        //When,Then
        mockMvc.perform(post("/api/v1/auth/phone/login/code")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.success").value(false));

        userRepository.delete(user);
    }

    /**
     * Convert object to JSON string.
     *
     * @param obj the object to convert
     * @return the JSON string representation of the object
     */
    private String asJsonString(Object obj) {
        try {
            return new ObjectMapper().writeValueAsString(obj);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
