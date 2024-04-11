package uk.gov.hmcts.reform.sendletter.tasks.reports;

import com.google.common.collect.ImmutableMap;
import jakarta.mail.internet.MimeMessage;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.mail.javamail.JavaMailSender;
import uk.gov.hmcts.reform.sendletter.services.FtpFileSummaryService;

import java.io.File;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SpringBootTest
class DailyFtpLetterUploadSummaryReportTest {

    @Mock
    private FtpFileSummaryService ftpFileSummaryService;

    @Value("${reports.ftp-uploaded-letters-summary.recipients}")
    private String[] recipients;

    @Autowired
    private EmailSender emailSender;

    @SpyBean
    private JavaMailSender mailSender;

    @Autowired
    private DailyFtpLetterUploadSummaryReport report;

    @Test
    void should_attempt_to_send_report_when_recipients_list_is_present() throws Exception {
        when(ftpFileSummaryService.getFtpFileUploadSummaryFiles())
            .thenReturn(ImmutableMap.of("a", new File("a.zip")));
        report = new DailyFtpLetterUploadSummaryReport(ftpFileSummaryService, emailSender, recipients);

        report.send();

        verify(mailSender).send(any(MimeMessage.class));
    }
}
