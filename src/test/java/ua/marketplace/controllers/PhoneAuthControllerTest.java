package ua.marketplace.controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.http.HttpServletRequest;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpSession;
import org.springframework.test.annotation.Rollback;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.transaction.annotation.Transactional;
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
@TestPropertySource(locations="classpath:application-dev.properties")
@Transactional
@SuppressWarnings("PMD")
class PhoneAuthControllerTest {

    @Autowired
    private MockMvc mockMvc;
    @Autowired
    private UserRepository userRepository;

    @Test
    @Rollback
    void testRegisterSuccessfully() throws Exception {

        RegistrationRequest request = new RegistrationRequest(
                "Test",
                "+380123456785");

        mockMvc.perform(post("/v1/auth/registration")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isAccepted())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.phoneNumber").value("+380123456785"));
    }

    @Test
    @Rollback
    void testRegistrationWithInvalidPatternRequest() throws Exception {

        RegistrationRequest request = new RegistrationRequest(
                "Test1",
                "+123456709812");

        mockMvc.perform(post("/v1/auth/registration")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage").exists())
                .andExpect(jsonPath("$.errorMessage.phoneNumber").value("Phone should contain only digits and should be in the format +380.."))
                .andExpect(jsonPath("$.errorMessage.firstName").value("Name should contain only letters (Latin or Cyrillic)"));
    }

    @Test
    @Rollback
    void testRegistrationWithInvalidSizeRequest() throws Exception {

        RegistrationRequest request = new RegistrationRequest(
                "s",
                "+3801");

        mockMvc.perform(post("/v1/auth/registration")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage").exists())
                .andExpect(jsonPath("$.errorMessage.phoneNumber").value("Phone should be between 13 digits"))
                .andExpect(jsonPath("$.errorMessage.firstName").value("Name should be between 2 and 15 characters"));
    }

    @Test
    @Rollback
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

        mockMvc.perform(post("/v1/auth/login/code")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.token").isNotEmpty());
    }

    @Test
    @Rollback
    void testLoginCodeInvalidPatternRequest() throws Exception {

        PhoneCodeRequest request = new PhoneCodeRequest(
                "12s5",
                "+123456789012");

        mockMvc.perform(post("/v1/auth/login/code")
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
    @Rollback
    void testLoginCodeInvalidSizeRequest() throws Exception {

        PhoneCodeRequest request = new PhoneCodeRequest(
                "112",
                "+38012");

        mockMvc.perform(post("/v1/auth/login/code")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage").exists())
                .andExpect(jsonPath("$.errorMessage.phoneNumber")
                        .value("Phone should be between 13 digits"))
                .andExpect(jsonPath("$.errorMessage.inputCode")
                        .value("Phone should be between 4 digits"));
    }


    @Test
    @Rollback
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

        mockMvc.perform(post("/v1/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON));
    }

    @Test
    @Rollback
    void testLoginFailed() throws Exception {

        PhoneNumberRequest request = new PhoneNumberRequest(
                "+123456789012");

        mockMvc.perform(post("/v1/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage").exists())
                .andExpect(jsonPath("$.errorMessage.phoneNumber")
                        .value("Phone should contain only digits and should be in the format +380.."));
    }

    @Test
    @Rollback
    void testLogoutSuccessfully() throws Exception {

        MockHttpSession session = new MockHttpSession();
        mockMvc.perform(post("/v1/auth/logout")
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