package ua.marketplace;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.TestPropertySource;
import org.springframework.transaction.annotation.Transactional;
import ua.marketplace.config.TestCacheConfig;
import ua.marketplace.entities.User;
import ua.marketplace.repositoryes.UserRepository;

@SpringBootTest
@AutoConfigureMockMvc
@TestPropertySource(locations="classpath:application-dev.properties")
@Import(TestCacheConfig.class)
@Transactional
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@WithMockUser(username = "test", password = "test")
public abstract class BaseTest {

    @Autowired
    private UserRepository userRepository;

    @BeforeEach
    public void setUp() {
        User user = new User();
        user.setPhoneNumber("testuser");
        user.setFirstName("password");
        // Заполните другие необходимые поля
        userRepository.save(user);
    }
}