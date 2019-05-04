package uk.gov.hmcts.reform.sendletter.config;

import com.microsoft.applicationinsights.TelemetryClient;
import com.microsoft.applicationinsights.telemetry.Duration;
import com.microsoft.applicationinsights.telemetry.RequestTelemetry;
import com.microsoft.applicationinsights.web.internal.RequestTelemetryContext;
import com.microsoft.applicationinsights.web.internal.ThreadContext;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;

import java.time.Instant;

@Aspect
@Configuration
public class AspectConfiguration {

    @Autowired(required = false)
    private TelemetryClient telemetryClient;

    @Around("@annotation(org.springframework.scheduling.annotation.Scheduled)")
    public void around(ProceedingJoinPoint joinPoint) throws Throwable {
        RequestTelemetryContext requestTelemetry = ThreadContext.getRequestTelemetryContext();
        boolean success = false;

        try {
            joinPoint.proceed();

            success = true;
        } finally {
            handleRequestTelemetry(requestTelemetry, joinPoint.getTarget().getClass().getSimpleName(), success);
        }
    }

    private void handleRequestTelemetry(
        RequestTelemetryContext requestTelemetryContext,
        String caller,
        boolean success
    ) {
        if (requestTelemetryContext != null) {
            handleRequestTelemetry(
                requestTelemetryContext.getHttpRequestTelemetry(),
                caller,
                requestTelemetryContext.getRequestStartTimeTicks(),
                success
            );
        }
    }

    private void handleRequestTelemetry(
        RequestTelemetry requestTelemetry,
        String caller,
        long start,
        boolean success
    ) {
        if (requestTelemetry != null) {
            requestTelemetry.setName("Schedule /" + caller);
            requestTelemetry.setDuration(new Duration(Instant.now().toEpochMilli() - start));
            requestTelemetry.setSuccess(success);

            if (telemetryClient != null) {
                telemetryClient.trackRequest(requestTelemetry);
            }
        }
    }
}
