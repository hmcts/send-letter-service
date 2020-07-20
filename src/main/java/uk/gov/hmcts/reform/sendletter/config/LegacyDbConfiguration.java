package uk.gov.hmcts.reform.sendletter.config;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.jdbc.DataSourceBuilder;
import org.springframework.boot.orm.jpa.EntityManagerFactoryBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;

import javax.persistence.EntityManagerFactory;
import javax.sql.DataSource;

@Configuration
@EnableTransactionManagement
@EnableJpaRepositories(
    entityManagerFactoryRef = "entityManagerFactory",
    basePackages = { "uk.gov.hmcts.reform.sendletter.entity" }
)
public class LegacyDbConfiguration {

    @Primary
    @Bean(name = "dataSource-legacy")
    @ConfigurationProperties(prefix = "spring.datasource.legacy")
    public DataSource dataSource() {
        return DataSourceBuilder.create().build();
    }

    @Primary
    @Bean(name = "entityManagerFactory-legacy")
    public LocalContainerEntityManagerFactoryBean entityManagerFactory(
        EntityManagerFactoryBuilder builder,
        @Qualifier("dataSource-legacy") DataSource dataSource
    ) {
        return builder
            .dataSource(dataSource)
            .packages(LetterRepository.class.getPackageName())
            .persistenceUnit("legacy")
            .build();
    }

    @Primary
    @Bean(name = "transactionManager-legacy")
    public PlatformTransactionManager transactionManager(
        @Qualifier("entityManagerFactory-legacy") EntityManagerFactory
            entityManagerFactory
    ) {
        return new JpaTransactionManager(entityManagerFactory);
    }
}
