package uk.gov.hmcts.reform.sendletter.logging;

import com.google.common.collect.ImmutableMap;
import com.microsoft.applicationinsights.TelemetryClient;
import com.microsoft.applicationinsights.telemetry.Duration;
import com.microsoft.applicationinsights.telemetry.RemoteDependencyTelemetry;
import com.microsoft.applicationinsights.telemetry.RequestTelemetry;
import com.microsoft.applicationinsights.web.internal.RequestTelemetryContext;
import com.microsoft.applicationinsights.web.internal.ThreadContext;
import com.microsoft.applicationinsights.web.internal.correlation.TelemetryCorrelationUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo;
import uk.gov.hmcts.reform.sendletter.model.ParsedReport;

import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.Map;

import static org.apache.commons.lang.StringUtils.EMPTY;
import static org.apache.commons.lang3.ObjectUtils.isNotEmpty;
import static uk.gov.hmcts.reform.sendletter.util.TimeZones.getCurrentEuropeLondonInstant;

/**
 * Aspect for Application Insights.
 */
@Aspect
@Component
public class AppInsights {

    private static final Logger log = LoggerFactory.getLogger(AppInsights.class);

    static final String LETTER_NOT_PRINTED = "LetterNotPrinted";

    static final String PENDING_LETTER = "PendingLetter";

    static final String LETTER_PRINT_REPORT = "LetterPrintReportReceived";

    static final DateTimeFormatter DATE_TIME_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    private final TelemetryClient telemetryClient = new TelemetryClient();


    public AppInsights() {

    }
    // schedules

    /**
     * Aspect for scheduled tasks.
     * @param joinPoint The join point
     * @throws Throwable The throwable error
     */
    @Around("@annotation(org.springframework.scheduling.annotation.Scheduled)")
    public void aroundSchedule(ProceedingJoinPoint joinPoint) throws Throwable {
        RequestTelemetryContext requestTelemetry = ThreadContext.getRequestTelemetryContext();
        boolean success = false;

        try {
            joinPoint.proceed();

            success = true;
        } finally {
            handleRequestTelemetry(requestTelemetry, joinPoint.getTarget().getClass().getSimpleName(), success);
        }
    }

    /**
     * Handle request telemetry.
     * @param requestTelemetryContext The request telemetry context
     * @param caller The caller
     * @param success The success
     */
    private void handleRequestTelemetry(
        RequestTelemetryContext requestTelemetryContext,
        String caller,
        boolean success
    ) {
        String requestName = "Schedule /" + caller;

        if (requestTelemetryContext != null) {
            handleRequestTelemetry(
                requestTelemetryContext.getHttpRequestTelemetry(),
                requestName,
                requestTelemetryContext.getRequestStartTimeTicks(),
                success
            );
        } else {
            log.warn(
                "Request Telemetry Context has been removed by ThreadContext - cannot log '{}' request",
                requestName
            );
        }
    }

    /**
     * Handle request telemetry.
     * @param requestTelemetry The request telemetry
     * @param requestName The request name
     * @param start The start
     * @param success The success
     */
    private void handleRequestTelemetry(
        RequestTelemetry requestTelemetry,
        String requestName,
        long start,
        boolean success
    ) {
        if (requestTelemetry != null) {
            requestTelemetry.setName(requestName);
            requestTelemetry.setDuration(new Duration(getCurrentEuropeLondonInstant().toEpochMilli() - start));
            requestTelemetry.setSuccess(success);

            telemetryClient.trackRequest(requestTelemetry);
        }
    }

    // dependencies

    /**
     * Use advice on dependency.
     * @param dependency The dependency
     */
    @Pointcut("@annotation(dependency)")
    public void useAdviceOnDependency(Dependency dependency) {
        // empty pointcut definition
    }

    /**
     * Around dependency.
     * @param joinPoint The join point
     * @param dependency The dependency
     * @return joinPoint.proceed() The join point proceed
     * @throws Throwable The throwable error
     */
    @Around("useAdviceOnDependency(dependency)")
    public Object aroundDependency(ProceedingJoinPoint joinPoint, Dependency dependency) throws Throwable {
        Instant start = Instant.now();
        boolean success = false;

        try {
            Object object = joinPoint.proceed();

            success = true;

            return object;
        } finally {
            handleDependencyTelemetry(dependency, ChronoUnit.MILLIS.between(start, Instant.now()), success);
        }
    }

    /**
     * Handle dependency telemetry.
     * @param dependency The dependency
     * @param durationInMillis The duration in milliseconds
     * @param success The success
     */
    private void handleDependencyTelemetry(Dependency dependency, long durationInMillis, boolean success) {
        // dependency definition
        RemoteDependencyTelemetry dependencyTelemetry = new RemoteDependencyTelemetry(
            dependency.name(),
            dependency.command(),
            new Duration(durationInMillis),
            success
        );

        dependencyTelemetry.setType(dependency.type());

        // tracing support
        RequestTelemetryContext context = ThreadContext.getRequestTelemetryContext();

        if (context != null) {
            RequestTelemetry requestTelemetry = context.getHttpRequestTelemetry();
            dependencyTelemetry.setId(TelemetryCorrelationUtils.generateChildDependencyId());
            dependencyTelemetry.getContext().getOperation().setId(
                requestTelemetry.getContext().getOperation().getId()
            );
            dependencyTelemetry.getContext().getOperation().setParentId(
                requestTelemetry.getId()
            );
        }

        String dependencyName = dependency.name();
        if (!dependencyName.startsWith("GET /health")
            || !dependencyName.contains("shedlock")
            || !dependencyName.equalsIgnoreCase("send_letter")) {
            telemetryClient.trackDependency(dependencyTelemetry);
        }
    }

    // events

    /**
     * Track stale letter.
     * @param staleLetter The stale letter
     */
    public void trackStaleLetter(BasicLetterInfo staleLetter) {
        Map<String, String> properties = new HashMap<>();

        properties.put("letterId", staleLetter.getId().toString());
        properties.put("checksum", staleLetter.getChecksum());
        properties.put("service", staleLetter.getService());
        properties.put("type", staleLetter.getType());
        properties.put("sentToPrintDayOfWeek", getSentToPrintOn(staleLetter));
        properties.put("sentToPrintAt", getSentToPrintAt(staleLetter));

        telemetryClient.trackEvent(LETTER_NOT_PRINTED, properties, null);
    }

    /**
     * Get sent to print at.
     * @param staleLetter The stale letter
     * @return date if not empty as string
     */
    private String getSentToPrintAt(BasicLetterInfo staleLetter) {
        return isNotEmpty(staleLetter.getSentToPrintAt())
            ? staleLetter.getSentToPrintAt().format(DATE_TIME_FORMAT)
            : EMPTY;
    }

    /**
     * Get sent to print on.
     * @param staleLetter The stale letter
     * @return day of week as string
     */
    private String getSentToPrintOn(BasicLetterInfo staleLetter) {
        return isNotEmpty(staleLetter.getSentToPrintAt())
            ? staleLetter.getSentToPrintAt().getDayOfWeek().name()
            : EMPTY;
    }

    /**
     * Track pending letter.
     * @param pendingLetter The pending letter
     */
    public void trackPendingLetter(BasicLetterInfo pendingLetter) {
        Map<String, String> properties = new HashMap<>();

        properties.put("letterId", pendingLetter.getId().toString());
        properties.put("service", pendingLetter.getService());
        properties.put("type", pendingLetter.getType());
        properties.put("createdAt", pendingLetter.getCreatedAt().format(DATE_TIME_FORMAT));
        properties.put("createdDayOfWeek", pendingLetter.getCreatedAt().getDayOfWeek().name());

        telemetryClient.trackEvent(PENDING_LETTER, properties, null);
    }

    /**
     * Track print report received.
     * @param report The report
     */
    public void trackPrintReportReceived(ParsedReport report) {
        telemetryClient.trackEvent(
            LETTER_PRINT_REPORT,
            ImmutableMap.of("isReportParsedFully", BooleanUtils.toStringYesNo(report.allRowsParsed)),
            ImmutableMap.of("reportSize", (double) report.statuses.size())
        );
    }
}
