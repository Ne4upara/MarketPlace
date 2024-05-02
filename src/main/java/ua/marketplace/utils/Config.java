package ua.marketplace.utils;

import io.micrometer.core.aop.CountedAspect;
import io.micrometer.core.aop.TimedAspect;
import io.micrometer.core.instrument.MeterRegistry;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;

/**
 * Configuration class for setting up aspects.
 */
@Configuration
@EnableAspectJAutoProxy(proxyTargetClass = true)
public class Config {

    /**
     * Configures the counted aspect.
     *
     * @param registry The MeterRegistry for counting.
     * @return CountedAspect object for counting.
     */
    @Bean
    CountedAspect countedAspect(MeterRegistry registry) {
        return new CountedAspect(registry);
    }

    /**
     * Configures the timed aspect.
     *
     * @param registry The MeterRegistry for timing.
     * @return TimedAspect object for timing.
     */
    @Bean
    public TimedAspect timedAspect(MeterRegistry registry) {
        return new TimedAspect(registry);
    }
}
