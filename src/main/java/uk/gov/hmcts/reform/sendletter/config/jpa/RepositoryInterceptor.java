package uk.gov.hmcts.reform.sendletter.config.jpa;

import org.aopalliance.intercept.MethodInterceptor;
import org.aopalliance.intercept.MethodInvocation;
import uk.gov.hmcts.reform.sendletter.logging.AppInsights;

import java.lang.reflect.Method;
import java.time.Duration;
import java.time.Instant;

class RepositoryInterceptor implements MethodInterceptor {

    private final AppInsights appInsights;

    RepositoryInterceptor(AppInsights appInsights) {
        this.appInsights = appInsights;
    }

    @Override
    public Object invoke(MethodInvocation invocation) throws Throwable {
        Method method = invocation.getMethod();

        String repository = method.getDeclaringClass().getSimpleName();
        String methodName = method.getName();

        Instant start = Instant.now();
        boolean success = false;

        try {
            Object object = invocation.proceed();

            success = true;

            return object;
        } catch (Throwable throwable) {
            appInsights.trackDependency(repository, methodName, Duration.between(start, Instant.now()), success);

            throw throwable;
        }
    }
}
