package ua.marketplace.utils;

import io.micrometer.core.aop.CountedAspect;
import io.micrometer.core.aop.TimedAspect;
import io.micrometer.core.instrument.MeterRegistry;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;

/**
 * This class is a Spring configuration class that enables aspect-oriented programming (AOP) for
 * counting and timing method executions using the Micrometer library.
 */
@Configuration
@EnableAspectJAutoProxy(proxyTargetClass = true) // enable support for @Aspect annotation
public class Config {

    /**
     * Creates a new CountedAspect bean that can be used to count the number of times a method is
     * executed. The MeterRegistry instance is used to register the counts.
     *
     * @param registry the MeterRegistry instance to use for registering counts
     * @return a new CountedAspect bean
     */
    @Bean
    CountedAspect countedAspect(MeterRegistry registry) {
        return new CountedAspect(registry);
    }

    /**
     * Creates a new TimedAspect bean that can be used to time the duration of method executions. The
     * MeterRegistry instance is used to register the timings.
     *
     * @param registry the MeterRegistry instance to use for registering timings
     * @return a new TimedAspect bean
     */
    @Bean
    public TimedAspect timedAspect(MeterRegistry registry) {
        return new TimedAspect(registry);
    }
}
