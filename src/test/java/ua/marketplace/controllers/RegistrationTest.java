package ua.marketplace.controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
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
class RegistrationTest {

    @Autowired
    private MockMvc mockMvc;
    @Autowired
    private UserRepository userRepository;
    @InjectMocks
    private PhoneAuthController regController;

    @Test
    void testRegisterSuccessfully() throws Exception {

        RegistrationRequest request = RegistrationRequest.builder()
                .phoneNumber("+380123456785")
                .firstName("Test")
                .build();

        mockMvc.perform(post("/api/v1/auth/registration")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isCreated())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.phoneNumber").value("+380123456785"));
    }

    @Test
    void testRegistrationWithInvalidRequest() throws Exception {

        RegistrationRequest request = RegistrationRequest
                .builder()
                .phoneNumber("+3801")
                .firstName("Test1")
                .build();

        mockMvc.perform(post("/api/v1/auth/registration")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage").exists())
                .andExpect(jsonPath("$.errorMessage.phoneNumber").value("Phone should be between 13 digits"))
                .andExpect(jsonPath("$.errorMessage.firstName").value("Name should contain only letters (Latin or Cyrillic)"));
    }

    @Test
    void testRegistrationWithInvalidTwoRequest() throws Exception {

        RegistrationRequest request = RegistrationRequest
                .builder()
                .phoneNumber("+123456709812")
                .firstName("s")
                .build();

        mockMvc.perform(post("/api/v1/auth/registration")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage").exists())
                .andExpect(jsonPath("$.errorMessage.phoneNumber").value("Phone should contain only digits and should be in the format +380.."))
                .andExpect(jsonPath("$.errorMessage.firstName").value("Name should be between 2 and 15 characters"));
    }

    @Test
    void testLoginCodeSuccessfully() throws Exception {

        PhoneCodeRequest request = PhoneCodeRequest.builder()
                .phoneNumber("+380123456784")
                .inputCode("1111")
                .build();

        User user = User.builder().phoneNumber(request.getPhoneNumber())
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
    void testLoginCodeFailed() throws Exception {

        PhoneCodeRequest request = PhoneCodeRequest
                .builder()
                .phoneNumber("+123456789012")
                .inputCode("12345")
                .build();

        mockMvc.perform(post("/api/v1/auth/login/code")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage").exists())
                .andExpect(jsonPath("$.errorMessage.phoneNumber")
                        .value("Phone should contain only digits and should be in the format +380.."))
                .andExpect(jsonPath("$.errorMessage.inputCode")
                        .value("Phone should be between 4 digits"));
    }

    @Test
    void testLoginCodeTwoFailed() throws Exception {

        PhoneCodeRequest request = PhoneCodeRequest
                .builder()
                .phoneNumber("+38012")
                .inputCode("11s2")
                .build();

        mockMvc.perform(post("/api/v1/auth/login/code")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage").exists())
                .andExpect(jsonPath("$.errorMessage.phoneNumber")
                        .value("Phone should be between 13 digits"))
                .andExpect(jsonPath("$.errorMessage.inputCode")
                        .value("Phone should contain only digits"));
    }

    @Test
    void testLoginSuccessfully() throws Exception {

        PhoneNumberRequest request = PhoneNumberRequest.builder()
                .phoneNumber("+380123467895")
                .build();

        User user = User.builder().phoneNumber(request.getPhoneNumber())
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
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.errorMessage")
                        .value("Time to send a repeat code 1 minute"));
    }

    @Test
    void testLoginFailed() throws Exception {

        PhoneNumberRequest request = PhoneNumberRequest
                .builder()
                .phoneNumber("+123456789012")
                .build();

        mockMvc.perform(post("/api/v1/auth/login")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.errorMessage").exists())
                .andExpect(jsonPath("$.errorMessage.phoneNumber")
                        .value("Phone should contain only digits and should be in the format +380.."));
    }

    private String asJsonString(Object obj) {
        try {
            return new ObjectMapper().writeValueAsString(obj);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}