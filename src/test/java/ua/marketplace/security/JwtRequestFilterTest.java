// Importing necessary classes for unit testing of JwtRequestFilter.
import ...

/**
 * Unit testing for the JwtRequestFilter class.
 * This class uses JUnit 5 testing framework along with Mockito for mocking
 * the dependencies of JwtRequestFilter.
 */
@SuppressWarnings("PMD") // Suppressing PMD warnings as per project requirement
@SpringBootTest
class JwtRequestFilterTest {

    // Creating mock objects for the dependencies of JwtRequestFilter
    @Mock
    private JwtUtil jwtUtil;
    @Mock
    private UserDetailsService service;
    @Mock

