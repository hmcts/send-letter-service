package uk.gov.hmcts.reform.sendletter.config;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;

/**
 * Adds custom error handler to Scheduled Tasks.
 */
@Configuration
public class ThreadPoolConfig {

    static int errorCount;
    static final Logger log = LoggerFactory.getLogger(ThreadPoolConfig.class);

    public static int getUnhandledTaskExceptionCount() {
        return errorCount;
    }

    @Bean
    public ThreadPoolTaskScheduler threadPoolTaskScheduler(
        @Value("${scheduling.pool}") int poolSize
    ) {
        ThreadPoolTaskScheduler threadPoolTaskScheduler = new ThreadPoolTaskScheduler();

        threadPoolTaskScheduler.setPoolSize(poolSize);
        threadPoolTaskScheduler.setThreadNamePrefix("SendLetterTask-");
        threadPoolTaskScheduler.setErrorHandler(t -> {
            log.error("Unhandled exception during task. {}: {}", t.getClass(), t.getMessage(), t);
            errorCount++;
        });

        return threadPoolTaskScheduler;
    }
}

