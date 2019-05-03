package uk.gov.hmcts.reform.sendletter.tasks.reports;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import uk.gov.hmcts.reform.sendletter.services.ReportsService;

import java.time.LocalDate;
import java.util.Collections;
import javax.mail.internet.MimeMessage;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class DailyLetterUploadSummaryReportTest {

    @Mock
    private JavaMailSender mailSender;

    @Mock
    private ReportsService reportService;

    private EmailSender emailSender;

    @BeforeEach
    void setUp() {
        emailSender = new EmailSender(mailSender, "unit@test");
    }

    @Test
    void should_send_a_report_to_single_recipient() {
        // given
        given(reportService.getCountFor(LocalDate.now())).willReturn(Collections.emptyList());
        given(mailSender.createMimeMessage()).willReturn(new JavaMailSenderImpl().createMimeMessage());
        doNothing().when(mailSender).send(any(MimeMessage.class));
        DailyLetterUploadSummaryReport report = new DailyLetterUploadSummaryReport(
            reportService,
            emailSender,
            new String[] { "test@localhost" }
        );

        // when
        report.send();

        // then
        verify(mailSender).send(any(MimeMessage.class));
    }

    @Test
    void should_attempt_to_send_a_report_when_no_recipients_are_configured() {
        // given
        given(reportService.getCountFor(LocalDate.now())).willReturn(Collections.emptyList());
        given(mailSender.createMimeMessage()).willReturn(new JavaMailSenderImpl().createMimeMessage());
        doNothing().when(mailSender).send(any(MimeMessage.class));
        DailyLetterUploadSummaryReport report = new DailyLetterUploadSummaryReport(reportService, emailSender, null);

        // when
        report.send();

        // then
        verify(mailSender).send(any(MimeMessage.class));
    }
}
