package uk.gov.hmcts.reform.sendletter.tasks.reports;

import jakarta.mail.internet.MimeMessage;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.test.context.bean.override.mockito.MockitoSpyBean;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;

@SpringBootTest
class DailyLetterUploadSummaryReportTest {

    @Autowired
    private DailyLetterUploadSummaryReport report;

    @MockitoSpyBean
    private JavaMailSender mailSender;

    @Test
    void should_attempt_to_send_report_when_recipients_list_is_present() {
        report.send();

        verify(mailSender).send(any(MimeMessage.class));
    }
}
