package ua.marketplace.swagger;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.enums.SecuritySchemeType;
import io.swagger.v3.oas.annotations.info.Info;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.security.SecurityScheme;
import org.springdoc.core.models.GroupedOpenApi;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * Configuration class for Swagger/OpenAPI documentation.
 */
@Configuration
@OpenAPIDefinition(
        info = @Info(
                title = "Marketplace",
                version = "1.0",
                description = "API documentation for marketplace application"
        ),
        security = {
                @SecurityRequirement(name = "bearer-key")
        }
)
@SecurityScheme(
        name = "bearer-key",
        type = SecuritySchemeType.HTTP,
        scheme = "bearer",
        bearerFormat = "JWT",
        description = "Authorization header using the Bearer scheme. Example: 'Bearer {token}'"
)
public class SwaggerConfig implements WebMvcConfigurer {

    /**
     * Configures Swagger/OpenAPI for the marketplace API.
     *
     * @return GroupedOpenApi object defining the API group, packages to scan, and paths to match.
     */
    @Bean
    public GroupedOpenApi customApi() {
        return GroupedOpenApi.builder()
                .group("marketplace")
                .packagesToScan("ua.marketplace.controllers")
                .pathsToMatch("/v1/**")
                .build();
    }
}
