package uk.gov.hmcts.reform.sendletter.logging;

import com.microsoft.applicationinsights.TelemetryClient;
import com.microsoft.applicationinsights.telemetry.Duration;
import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.logging.appinsights.AbstractAppInsights;
import uk.gov.hmcts.reform.sendletter.entity.StaleLetter;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

@Component
public class AppInsights extends AbstractAppInsights {

    static final String SERVICE_BUS_DEPENDENCY = "ServiceBus";
    static final String SERVICE_BUS_MESSAGE_ACKNOWLEDGED = "MessageAcknowledged";

    static final String LETTER_NOT_PRINTED = "LetterNotPrinted";

    public AppInsights(TelemetryClient telemetry) {
        super(telemetry);
    }

    public void trackMessageAcknowledgement(java.time.Duration duration, boolean success, String messageId) {
        telemetry.trackDependency(
            SERVICE_BUS_DEPENDENCY,
            SERVICE_BUS_MESSAGE_ACKNOWLEDGED,
            new Duration(duration.toMillis()),
            success
        );

        if (success) {
            telemetry.trackEvent(
                SERVICE_BUS_MESSAGE_ACKNOWLEDGED,
                Collections.singletonMap("messageId", messageId),
                null
            );
        }
    }

    public void trackFtpUpload(java.time.Duration duration, boolean success) {
        telemetry.trackDependency(
            AppDependency.FTP_CLIENT,
            AppDependencyCommand.FTP_FILE_UPLOADED,
            new Duration(duration.toMillis()),
            success
        );
    }

    public void trackNotPrintedLetter(StaleLetter staleLetter) {
        LocalDateTime sentToPrint = staleLetter.getSentToPrintAt().toLocalDateTime();
        Map<String, String> properties = new HashMap<>();

        properties.put("letterId", staleLetter.getId().toString());
        properties.put("messageId", staleLetter.getMessageId());
        properties.put("service", staleLetter.getService());
        properties.put("type", staleLetter.getType());
        properties.put("weekday", sentToPrint.getDayOfWeek().name());
        properties.put("sentToPrintAt", sentToPrint.toLocalTime().toString());

        telemetry.trackEvent(LETTER_NOT_PRINTED, properties, null);
    }

    public void trackException(Exception exception) {
        telemetry.trackException(exception);
    }
}
