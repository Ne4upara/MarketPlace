package ua.marketplace.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.cache.RedisCacheManager;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import java.time.Duration;

@Configuration // Indicates that this class contains Spring bean definitions
public class RedisCacheConfig {

    @Bean // Declares a method to produce a bean to be managed by the Spring container
    public RedisCacheManager cacheManager(RedisConnectionFactory connectionFactory) {
        RedisCacheConfiguration cacheConfiguration = RedisCacheConfiguration.defaultCacheConfig() // Creates a Redis cache configuration with default settings
            .entryTtl(Duration.ofMinutes(1)); // Sets the time-to-live (TTL) for cache entries to 1 minute

        return RedisCacheManager.builder(connectionFactory) // Creates a Redis cache manager builder
            .cacheDefaults(cacheConfiguration) // Sets the default cache configuration
            .build(); // Builds and returns the Redis cache manager
    }
}
