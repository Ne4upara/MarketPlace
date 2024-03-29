package ua.marketplace.controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.http.HttpServletRequest;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import ua.marketplace.entities.User;
import ua.marketplace.entities.VerificationCode;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.PhoneCodeRequest;
import ua.marketplace.requests.PhoneNumberRequest;
import ua.marketplace.requests.RegistrationRequest;

import java.time.LocalDateTime;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;


@SpringBootTest
@AutoConfigureMockMvc
@SuppressWarnings("PMD")
class PhoneAuthControllerTest {

    @Autowired
    private MockMvc mockMvc;
    @Autowired
    private UserRepository userRepository;

    @Test
    void testRegisterSuccessfully() throws Exception {

        RegistrationRequest request = new RegistrationRequest(
                "Test",
                "+380123456785");

        mockMvc.perform(post("/api/v1/auth/registration")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isAccepted())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.phoneNumber").value("+380123456785"));
    }

    @Test
    void testRegistrationWithInvalidPatternRequest() throws Exception {

        RegistrationRequest request = new RegistrationRequest(
                "Test1",
                "+123456709812");

        mockMvc.perform(post("/api/v1/auth/registration")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage").exists())
                .andExpect(jsonPath("$.errorMessage.phoneNumber").value("Phone should contain only digits and should be in the format +380.."))
                .andExpect(jsonPath("$.errorMessage.firstName").value("Name should contain only letters (Latin or Cyrillic)"));
    }

    @Test
    void testRegistrationWithInvalidSizeRequest() throws Exception {

        RegistrationRequest request = new RegistrationRequest(
                "s",
                "+3801");

        mockMvc.perform(post("/api/v1/auth/registration")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage").exists())
                .andExpect(jsonPath("$.errorMessage.phoneNumber").value("Phone should be between 13 digits"))
                .andExpect(jsonPath("$.errorMessage.firstName").value("Name should be between 2 and 15 characters"));
    }

    @Test
    void testLoginCodeSuccessfully() throws Exception {

        PhoneCodeRequest request = new PhoneCodeRequest(
                "1111",
                "+380123456784");

        User user = User.builder().phoneNumber(request.phoneNumber())
                .firstName("Test").build();
        VerificationCode code = VerificationCode.builder()
                .code("1111")
                .createdTimeCode(LocalDateTime.now())
                .user(user)
                .build();
        user.setVerificationCode(code);
        userRepository.save(user);

        mockMvc.perform(post("/api/v1/auth/login/code")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.firstName").value("Test"))
                .andExpect(jsonPath("$.token").isNotEmpty());
    }

    @Test
    void testLoginCodeInvalidPatternRequest() throws Exception {

        PhoneCodeRequest request = new PhoneCodeRequest(
                "12s5",
                "+123456789012");

        mockMvc.perform(post("/api/v1/auth/login/code")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage").exists())
                .andExpect(jsonPath("$.errorMessage.phoneNumber")
                        .value("Phone should contain only digits and should be in the format +380.."))
                .andExpect(jsonPath("$.errorMessage.inputCode")
                        .value("Phone should contain only digits"));
    }

    @Test
    void testLoginCodeInvalidSizeRequest() throws Exception {

        PhoneCodeRequest request = new PhoneCodeRequest(
                "112",
                "+38012");

        mockMvc.perform(post("/api/v1/auth/login/code")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage").exists())
                .andExpect(jsonPath("$.errorMessage.phoneNumber")
                        .value("Phone should be between 13 digits"))
                .andExpect(jsonPath("$.errorMessage.inputCode")
                        .value("Phone should be between 4 digits"));
    }

    /**
     * The test does not work correctly on H2 memory database.
     * For a Postgres database, apply the following conditions:
     * .andExpect(status().isOk())
     * .andExpect(content().contentType(MediaType.APPLICATION_JSON))
     * .andExpect(jsonPath("$.phoneNumber").value(request.getPhoneNumber()));
     */
    @Test
    void testLoginSuccessfully() throws Exception {

        PhoneNumberRequest request = new PhoneNumberRequest(
                "+380123467895");

        User user = User.builder().phoneNumber(request.phoneNumber())
                .firstName("Test").build();
        VerificationCode code = VerificationCode.builder()
                .code("1111")
                .createdTimeCode(LocalDateTime.now().minusMinutes(1))
                .user(user)
                .build();
        user.setVerificationCode(code);
        userRepository.save(user);

        mockMvc.perform(post("/api/v1/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isConflict())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.errorMessage")
                        .value("Time to send a repeat code 1 minute"));
    }

    @Test
    void testLoginFailed() throws Exception {

        PhoneNumberRequest request = new PhoneNumberRequest(
                "+123456789012");

        mockMvc.perform(post("/api/v1/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage").exists())
                .andExpect(jsonPath("$.errorMessage.phoneNumber")
                        .value("Phone should contain only digits and should be in the format +380.."));
    }

    @Test
    void testLogoutSuccessfully() throws Exception {

        MockHttpSession session = new MockHttpSession();
        HttpServletRequest request = MockMvcRequestBuilders.post("/logout")
                .header("Authorization", "Bearer your_token")
                .session(session)
                .buildRequest(session.getServletContext());

        mockMvc.perform(post("/api/v1/auth/logout")
                        .session(session))
                .andExpect(status().isNoContent());
    }

    private String asJsonString(Object obj) {
        try {
            return new ObjectMapper().writeValueAsString(obj);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}