package uk.gov.hmcts.reform.sendletter.tasks.reports;

import com.google.common.collect.ImmutableMap;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.services.FtpFileSummaryService;

import java.io.File;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static uk.gov.hmcts.reform.sendletter.tasks.reports.DailyFtpLetterUploadSummaryReport.ATTACHMENT_NAME_FORMAT;
import static uk.gov.hmcts.reform.sendletter.tasks.reports.DailyFtpLetterUploadSummaryReport.EMAIL_SUBJECT;

@ExtendWith(MockitoExtension.class)
class DailyFtpLetterUploadSummaryReportTest {

    @Mock
    private FtpFileSummaryService ftpFileSummaryService;

    @Mock
    private EmailSender emailSender;

    @Captor
    ArgumentCaptor<Attachment> attachmentArgumentCaptor;

    @Captor
    ArgumentCaptor<String> emailSubjectCaptor;

    @Captor
    ArgumentCaptor<String[]> recipientsCaptor;

    private DailyFtpLetterUploadSummaryReport dailyFtpLetterUploadSummaryReport;

    String[] recipients = null;
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");

    @Test
    void should_send_email_for_all_recipients() {
        // given
        recipients = new String[]{"test1@dummy.com", "test2@dummy.com"};
        dailyFtpLetterUploadSummaryReport = new DailyFtpLetterUploadSummaryReport(
            ftpFileSummaryService, emailSender, recipients);

        File serviceFile1 = new File("service1-file");
        File serviceFile2 = new File("service2-file");
        given(ftpFileSummaryService.getFtpFileUploadSummaryFiles())
            .willReturn(ImmutableMap.of("service1", serviceFile1, "service2", serviceFile2));

        // when
        dailyFtpLetterUploadSummaryReport.send();

        // then
        verify(emailSender).send(emailSubjectCaptor.capture(), recipientsCaptor.capture(),
            attachmentArgumentCaptor.capture());
        assertThat(emailSubjectCaptor.getValue()).isEqualTo(EMAIL_SUBJECT);
        assertThat(recipientsCaptor.getValue()).isEqualTo(recipients);
        assertThat(attachmentArgumentCaptor.getAllValues()).hasSize(2)
            .extracting(attachment -> tuple(attachment.filename, attachment.file))
            .containsExactlyInAnyOrder(
                tuple(generateFileNameFor("service1"), serviceFile1),
                tuple(generateFileNameFor("service2"), serviceFile2));
    }

    @Test
    void should_not_send_a_report_when_no_recipients_are_configured() {
        // given
        recipients = new String[]{};
        dailyFtpLetterUploadSummaryReport = new DailyFtpLetterUploadSummaryReport(
            ftpFileSummaryService, emailSender, recipients);

        // when
        dailyFtpLetterUploadSummaryReport.send();

        // then
        verify(ftpFileSummaryService, never()).getFtpFileUploadSummaryFiles();
        verify(emailSender, never()).send(anyString(), any(), any(Attachment.class));
    }

    private String generateFileNameFor(String service) {
        return String.format(ATTACHMENT_NAME_FORMAT, service, LocalDate.now().format(FORMATTER));
    }
}
