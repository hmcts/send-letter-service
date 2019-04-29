package uk.gov.hmcts.reform.sendletter.config.jpa;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.support.JpaRepositoryFactoryBean;
import org.springframework.data.repository.core.support.RepositoryFactorySupport;
import uk.gov.hmcts.reform.sendletter.logging.AppInsights;

import java.io.Serializable;
import javax.persistence.EntityManager;

class CustomRepositoryFactoryBean<R extends JpaRepository<T, I>, T, I extends Serializable>
    extends JpaRepositoryFactoryBean<R, T, I> {

    @Autowired
    private AppInsights appInsights;

    public CustomRepositoryFactoryBean(Class<? extends R> repositoryInterface) {
        super(repositoryInterface);
    }

    @Override
    protected RepositoryFactorySupport createRepositoryFactory(EntityManager em) {
        RepositoryFactorySupport factory = super.createRepositoryFactory(em);
        factory.addRepositoryProxyPostProcessor(new JpaAccessorProxyPostProcessor(appInsights));
        return factory;
    }
}
