package uk.gov.hmcts.reform.sendletter.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.Clock;

/**
 * Configuration for time.
 */
@Configuration
public class TimeConfiguration {

    public static final String DATE_TIME_PATTERN = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'";

    /**
     * Create a Clock.
     * @return The Clock
     */
    @Bean
    public Clock clock() {
        return Clock.systemDefaultZone();
    }

}
