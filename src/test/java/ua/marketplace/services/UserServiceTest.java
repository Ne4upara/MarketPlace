package ua.marketplace.services;

import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.springframework.boot.test.context.SpringBootTest;
import ua.marketplace.dto.UserDto;
import ua.marketplace.entitys.User;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

/**
 * Unit tests for the UserService class.
 */
@SpringBootTest
class UserServiceTest {

    @InjectMocks
    private UserService service;

    /**
     * Test for the convertToDto method of the UserService class.
     * It verifies that a User entity object can be successfully converted to a UserDto object.
     */
    @Test
    void testConvertToDto() {

        //Given
        User user = User
                .builder()
                .phone("111111111")
                .build();

        UserDto expect = UserDto
                .builder()
                .phone(user.getPhone())
                .build();

        //When
        UserDto result = service.convertToDto(user);

        //Then
        assertThat(result).isEqualTo(expect);
    }
}
