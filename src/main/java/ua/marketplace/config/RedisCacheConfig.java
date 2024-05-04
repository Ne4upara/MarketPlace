package ua.marketplace.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.cache.RedisCacheManager;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import java.time.Duration;

/**
 * Configuration class for setting up Redis cache management.
 * Indicates that this class contains Spring bean definitions.
 */
@Configuration
public class RedisCacheConfig {

    /**
     * Defines a Redis cache manager bean to be managed by the Spring container.
     *
     * @param connectionFactory The Redis connection factory to use for cache operations.
     * @return A RedisCacheManager configured with default cache settings.
     */
    @Bean
    public RedisCacheManager cacheManager(RedisConnectionFactory connectionFactory) {
        RedisCacheConfiguration cacheConfiguration = RedisCacheConfiguration.defaultCacheConfig()
                .entryTtl(Duration.ofMinutes(1));

        return RedisCacheManager.builder(connectionFactory)
                .cacheDefaults(cacheConfiguration)
                .build();
    }
}
