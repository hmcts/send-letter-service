package uk.gov.hmcts.reform.sendletter.logging;

import com.microsoft.applicationinsights.TelemetryClient;
import com.microsoft.applicationinsights.telemetry.Duration;
import com.microsoft.applicationinsights.telemetry.TelemetryContext;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import uk.gov.hmcts.reform.authorisation.exceptions.InvalidTokenException;
import uk.gov.hmcts.reform.sendletter.entity.Letter;

import java.sql.Timestamp;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class AppInsightsTest {

    private static final java.time.Duration TIME_TOOK = java.time.Duration.ofMinutes(1);

    private static final String MESSAGE_ID = "some-message-id";

    private static final String SERVICE_NAME = "some-service-name";

    private static final String TYPE = "some-type";

    @Captor
    private ArgumentCaptor<Map<String, String>> properties;

    @Mock
    private TelemetryClient telemetry;

    private AppInsights insights;

    private final TelemetryContext context = new TelemetryContext();

    @Before
    public void setUp() {
        context.setInstrumentationKey("some-key");
        when(telemetry.getContext()).thenReturn(context);
        insights = new AppInsights(telemetry);
    }

    @After
    public void tearDown() {
        reset(telemetry);
    }

    // dependencies

    @Test
    public void should_track_service_authentication() {
        insights.trackServiceAuthentication(() -> "service name");
        Throwable exception = catchThrowable(() -> insights.trackServiceAuthentication(() -> {
            throw new InvalidTokenException("dummy token", null);
        }));

        assertThat(exception).isInstanceOf(InvalidTokenException.class);
        verify(telemetry, times(2)).trackDependency(
            eq(AppDependency.AUTH_SERVICE),
            eq(AppDependencyCommand.AUTH_SERVICE_HEADER),
            any(Duration.class),
            anyBoolean()
        );
    }

    @Test
    public void should_track_ftp_upload() {
        insights.trackFtpUpload(TIME_TOOK, true);
        insights.trackFtpUpload(TIME_TOOK, false);

        verify(telemetry, times(2)).trackDependency(
            eq(AppDependency.FTP_CLIENT),
            eq(AppDependencyCommand.FTP_FILE_UPLOADED),
            any(Duration.class),
            anyBoolean()
        );
    }

    @Test
    public void should_track_ftp_download() {
        insights.trackFtpReportsDownload(TIME_TOOK, true);
        insights.trackFtpReportsDownload(TIME_TOOK, false);

        verify(telemetry, times(2)).trackDependency(
            eq(AppDependency.FTP_CLIENT),
            eq(AppDependencyCommand.FTP_DOWNLOAD_REPORTS),
            any(Duration.class),
            anyBoolean()
        );
    }

    @Test
    public void should_track_ftp_delete() {
        insights.trackFtpReportDelete(TIME_TOOK, true);
        insights.trackFtpReportDelete(TIME_TOOK, false);

        verify(telemetry, times(2)).trackDependency(
            eq(AppDependency.FTP_CLIENT),
            eq(AppDependencyCommand.FTP_REPORT_DELETE),
            any(Duration.class),
            anyBoolean()
        );
    }

    // events

    @Test
    public void should_track_event_of_not_printed_letter() {
        Letter letter = new Letter(UUID.randomUUID(), MESSAGE_ID, SERVICE_NAME, null, TYPE, null);
        ZonedDateTime sentToPrint = ZonedDateTime.now(ZoneOffset.UTC);
        letter.setSentToPrintAt(Timestamp.from(sentToPrint.toInstant()));

        insights.trackStaleLetter(letter);

        Map<String, String> expectedProperties = new HashMap<>();
        expectedProperties.put("letterId", letter.getId().toString());
        expectedProperties.put("messageId", MESSAGE_ID);
        expectedProperties.put("service", SERVICE_NAME);
        expectedProperties.put("type", TYPE);
        expectedProperties.put("sentToPrintDayOfWeek", sentToPrint.getDayOfWeek().name());
        expectedProperties.put("sentToPrintAt", sentToPrint.format(AppInsights.TIME_FORMAT));

        verify(telemetry).trackEvent(
            eq(AppInsights.LETTER_NOT_PRINTED),
            properties.capture(),
            eq(null)
        );
        assertThat(properties.getValue()).containsAllEntriesOf(expectedProperties);
    }

    @Test
    public void should_track_exception() {
        insights.trackException(new NullPointerException("Some null"));

        verify(telemetry).trackException(any(NullPointerException.class));
    }
}
