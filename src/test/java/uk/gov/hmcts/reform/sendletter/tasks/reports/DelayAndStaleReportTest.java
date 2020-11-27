package uk.gov.hmcts.reform.sendletter.tasks.reports;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.services.DelayedPrintService;
import uk.gov.hmcts.reform.sendletter.services.StaleLetterService;

import java.io.File;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static uk.gov.hmcts.reform.sendletter.tasks.reports.DelayAndStaleReport.EMAIL_SUBJECT;

@ExtendWith(MockitoExtension.class)
class DelayAndStaleReportTest {

    @Mock
    private DelayedPrintService delayedPrintService;

    @Mock
    private StaleLetterService staleLetterService;

    @Mock
    private EmailSender emailSender;

    private DelayAndStaleReport delayAndStaleReport;

    String[] recipients = null;


    @BeforeEach
    void setUp() {
        recipients = new String[]{"test@dummy.com", "test_2@dummy.com"};
        delayAndStaleReport = new DelayAndStaleReport(delayedPrintService, staleLetterService, emailSender,
                recipients, 2);
    }

    @Test
    void should_send_emails() throws IOException {
        File deplayedFile = new File("delayed-file");
        given(delayedPrintService.getDeplayLettersAttachment(isA(LocalDateTime.class),
                isA(LocalDateTime.class), anyInt())).willReturn(deplayedFile);

        delayAndStaleReport.send();

        verify(delayedPrintService).getDeplayLettersAttachment(isA(LocalDateTime.class),
                isA(LocalDateTime.class), anyInt());
        ArgumentCaptor<String> emailSubjectCaptor = ArgumentCaptor.forClass(String.class);
        ArgumentCaptor<String[]> recipientsCaptor = ArgumentCaptor.forClass(String[].class);
        ArgumentCaptor<Attachment> attachmentArgumentCaptor = ArgumentCaptor.forClass(Attachment.class);

        verify(emailSender).send(emailSubjectCaptor.capture(), recipientsCaptor.capture(),
                attachmentArgumentCaptor.capture());

        assertThat(emailSubjectCaptor.getValue()).isEqualTo(EMAIL_SUBJECT);
        assertThat(recipientsCaptor.getValue()).isEqualTo(recipients);
        assertThat(attachmentArgumentCaptor.getValue()).isNotNull();
    }

    @ParameterizedTest
    @MethodSource("emailRecipients")
    void should_not_invoke_send_emails_for_empty_recipients(final String[] data) throws IOException {
        delayAndStaleReport = new DelayAndStaleReport(delayedPrintService, staleLetterService,
                emailSender, data, 2);

        delayAndStaleReport.send();

        verify(delayedPrintService, never()).getDeplayLettersAttachment(isA(LocalDateTime.class),
                isA(LocalDateTime.class), anyInt());
        verify(emailSender, never()).send(eq(EMAIL_SUBJECT), eq(recipients),
                ArgumentMatchers.<Attachment>any());
    }

    static Stream<Arguments> emailRecipients() {
        return Stream.of(
                Arguments.of((Object) null),
                Arguments.of((Object) new String[]{})
        );
    }

    @Test
    void should_not_invoke_send_emails_for_exception() throws IOException {
        given(delayedPrintService.getDeplayLettersAttachment(isA(LocalDateTime.class),
                isA(LocalDateTime.class), anyInt())).willThrow(new RuntimeException("Error occured"));

        delayAndStaleReport.send();

        verify(emailSender, never()).send(eq(EMAIL_SUBJECT), eq(recipients),
                ArgumentMatchers.<Attachment>any());
    }





}