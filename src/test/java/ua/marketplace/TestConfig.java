package ua.marketplace;

import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.cache.CacheManager;
import org.springframework.cache.concurrent.ConcurrentMapCacheManager;
import software.amazon.awssdk.services.s3.S3Client;

@TestConfiguration
public class TestConfig {

    @MockBean
    private RedisTemplate<String, Object> redisTemplate;

    @MockBean
    private S3Client amazonS3;

//    @Bean
//    @Primary
//    public CacheManager cacheManager() {
//        return new ConcurrentMapCacheManager();
//    }
}
