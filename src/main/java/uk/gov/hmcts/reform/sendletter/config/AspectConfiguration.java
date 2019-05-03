package uk.gov.hmcts.reform.sendletter.config;

import com.microsoft.applicationinsights.TelemetryClient;
import com.microsoft.applicationinsights.telemetry.Duration;
import com.microsoft.applicationinsights.telemetry.RequestTelemetry;
import com.microsoft.applicationinsights.web.internal.ThreadContext;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;

import java.time.Instant;
import java.time.temporal.ChronoUnit;

@Aspect
@Configuration
public class AspectConfiguration {

    @Autowired(required = false)
    private TelemetryClient telemetryClient;

    @Around("@annotation(org.springframework.scheduling.annotation.Scheduled)")
    public void around(ProceedingJoinPoint joinPoint) throws Throwable {
        RequestTelemetry requestTelemetry = ThreadContext.getRequestTelemetryContext().getHttpRequestTelemetry();
        Instant start = Instant.now();
        boolean success = false;

        if (requestTelemetry != null) {
            requestTelemetry.setName("Schedule /" + joinPoint.getTarget().getClass().getSimpleName());
        }

        try {
            joinPoint.proceed();

            success = true;
        } finally {
            if (requestTelemetry != null) {
                requestTelemetry.setDuration(new Duration(ChronoUnit.MILLIS.between(start, Instant.now())));
                requestTelemetry.setSuccess(success);

                if (telemetryClient != null) {
                    telemetryClient.trackRequest(requestTelemetry);
                }
            }
        }
    }
}
