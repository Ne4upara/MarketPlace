package ua.marketplace.controllers;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.support.DefaultMessageSourceResolvable;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.validation.BindingResult;
import org.springframework.validation.MapBindingResult;
import ua.marketplace.dto.CodeDto;
import ua.marketplace.dto.PhoneNumberDto;
import ua.marketplace.entities.User;
import ua.marketplace.repositoryes.UserRepository;
import ua.marketplace.requests.PhoneCodeRequest;
import ua.marketplace.requests.PhoneNumberRequest;
import ua.marketplace.responses.CustomResponse;
import ua.marketplace.services.PhoneNumberRegistrationService;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * Test class for AuthController, which handles authentication-related endpoints.
 */
@SpringBootTest
@AutoConfigureMockMvc
class RegistrationTest {

    @Autowired
    private MockMvc mockMvc;
    @Mock
    private PhoneNumberRegistrationService regService;
    @Mock
    private UserRepository userRepository;
    @InjectMocks
    private RegistrationController regController;

    /**
     * Test for successful user registration.
     *
     * @throws Exception if an error occurs during the test
     */
    @Test
    void testRegisterSuccessfully() throws Exception {

        //Given
        PhoneNumberRequest request = PhoneNumberRequest.builder()
                .phoneNumber("+123456789012")
                .build();

        User user = User
                .builder()
                .phone(request.getPhoneNumber())
                .build();

        BindingResult bindingResult = new MapBindingResult(Collections.emptyMap(), "");

        ResponseEntity<CustomResponse<PhoneNumberDto>> expect = ResponseEntity
                .ok()
                .body(CustomResponse.successfully(new PhoneNumberDto(),
                        HttpStatus.OK.value()));

        when(regService.inputPhoneNumber(request)).thenReturn(expect);
        when(userRepository.findByPhone(request.getPhoneNumber())).thenReturn(Optional.of(user));

        //When
        ResponseEntity<CustomResponse<PhoneNumberDto>> result = regController
                .inputPhoneNumber(request, bindingResult);

        //Then
        Assertions.assertEquals(expect, result);
        mockMvc.perform(post("/api/v1/auth//phonenumber/registration")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.success").value(true));
    }

    /**
     * Test for registration with invalid request data.
     *
     * @throws Exception if an error occurs during the test
     */
    @Test
    void testRegistrationWithInvalidRequest() throws Exception {

        //Given
        PhoneNumberRequest request = PhoneNumberRequest
                .builder()
                .phoneNumber("")
                .build();

        User user = User
                .builder()
                .phone(request.getPhoneNumber())
                .build();

        BindingResult bindingResult = new MapBindingResult(Collections.emptyMap(), "");

        List<String> errorList = bindingResult.getAllErrors().stream()
                .map(DefaultMessageSourceResolvable::getDefaultMessage)
                .toList();

        ResponseEntity<CustomResponse<PhoneNumberDto>> expect = ResponseEntity
                .badRequest()
                .body(CustomResponse.failed(errorList,
                        HttpStatus.BAD_REQUEST.value()));

        when(regService.inputPhoneNumber(request)).thenReturn(expect);
        when(userRepository.findByPhone(request.getPhoneNumber())).thenReturn(Optional.of(user));

        //When
        ResponseEntity<CustomResponse<PhoneNumberDto>> result = regController.inputPhoneNumber(request, bindingResult);

        //Then
        Assertions.assertEquals(expect, result);
        mockMvc.perform(post("/api/v1/auth//phonenumber/registration")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.success").value(false));
    }

    /**
     * Test for user login.
     *
     * @throws Exception if an error occurs during the test
     */
    @Test
    void testLogin() throws Exception {

        //Given
        PhoneCodeRequest request = PhoneCodeRequest
                .builder()
                .phoneNumber("111111111")
                .inputCode("999999")
                .build();

        BindingResult bindingResult = new MapBindingResult(Collections.emptyMap(), "");
        ResponseEntity<CustomResponse<CodeDto>> expect = ResponseEntity
                .ok()
                .body(CustomResponse.successfully(new CodeDto(),
                        HttpStatus.OK.value()));

        when(userRepository.findByPhone(request.getPhoneNumber())).thenReturn(Optional.of(new User()));
        when(regService.inputPhoneCode(request)).thenReturn(expect);

        //When
        ResponseEntity<CustomResponse<CodeDto>> result = regController.inputCode(request, bindingResult);

        //Then
        Assertions.assertEquals(expect, result);
        mockMvc.perform(post("/api/v1/auth/phonenumber/code/registration")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(asJsonString(request)))
                .andExpect(status().isBadRequest())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.success").value(false));
    }

    /**
     * Helper method to convert object to JSON string.
     *
     * @param obj the object to convert
     * @return JSON string representation of the object
     */
    private String asJsonString(Object obj) {
        try {
            return new ObjectMapper().writeValueAsString(obj);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}