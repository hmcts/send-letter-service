package uk.gov.hmcts.reform.sendletter.config.jpa;

import org.springframework.aop.framework.ProxyFactory;
import org.springframework.data.repository.core.RepositoryInformation;
import org.springframework.data.repository.core.support.RepositoryProxyPostProcessor;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.logging.AppInsights;

class JpaAccessorProxyPostProcessor implements RepositoryProxyPostProcessor {

    private final AppInsights appInsights;

    JpaAccessorProxyPostProcessor(AppInsights appInsights) {
        this.appInsights = appInsights;
    }

    @Override
    public void postProcess(ProxyFactory factory, RepositoryInformation repositoryInformation) {
        if (repositoryInformation.getRepositoryInterface().equals(LetterRepository.class)) {
            factory.addAdvice(new RepositoryInterceptor(appInsights));
        }
    }
}
