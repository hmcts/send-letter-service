package uk.gov.hmcts.reform.sendletter.config;

import net.javacrumbs.shedlock.core.LockProvider;
import net.javacrumbs.shedlock.provider.jdbctemplate.JdbcTemplateLockProvider;
import net.javacrumbs.shedlock.spring.annotation.EnableSchedulerLock;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;

import javax.sql.DataSource;

/**
 * Configuration for Scheduler.
 */
@Configuration
@AutoConfigureAfter(FlywayConfiguration.class)
@DependsOn({"flyway", "flywayInitializer"})
@ConditionalOnProperty(value = "scheduling.enabled", matchIfMissing = true)
@EnableSchedulerLock(defaultLockAtMostFor = "${scheduling.lock_at_most_for}")
public class SchedulerConfiguration {

    /**
     * Create a LockProvider.
     * @param dataSource The data source
     * @return The LockProvider
     */
    @Bean
    public LockProvider lockProvider(DataSource dataSource) {
        return new JdbcTemplateLockProvider(dataSource);
    }
}
