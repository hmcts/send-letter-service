package uk.gov.hmcts.reform.sendletter.config.jpa;

import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import uk.gov.hmcts.reform.sendletter.logging.AppInsights;

@Configuration
@ConditionalOnBean(AppInsights.class)
@EnableJpaRepositories(repositoryFactoryBeanClass = CustomRepositoryFactoryBean.class)
public class RepositoryConfiguration {
}
