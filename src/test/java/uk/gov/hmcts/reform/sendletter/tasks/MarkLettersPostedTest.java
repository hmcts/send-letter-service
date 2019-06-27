package uk.gov.hmcts.reform.sendletter.tasks;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.gov.hmcts.reform.sendletter.SampleData;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;
import uk.gov.hmcts.reform.sendletter.logging.AppInsights;
import uk.gov.hmcts.reform.sendletter.model.Report;
import uk.gov.hmcts.reform.sendletter.services.ReportParser;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpAvailabilityChecker;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpClient;

import java.util.Optional;
import java.util.UUID;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class MarkLettersPostedTest {

    @Mock LetterRepository repo;
    @Mock FtpClient ftpClient;
    @Mock FtpAvailabilityChecker availabilityChecker;
    @Mock ReportParser parser;
    @Mock AppInsights insights;

    private MarkLettersPostedTask task;

    @BeforeEach
    void setup() {
        task = new MarkLettersPostedTask(repo, ftpClient, availabilityChecker, parser, insights);
    }

    @Test
    void continues_processing_if_letter_not_found() {
        String filePath = "a.csv";
        UUID known = UUID.randomUUID();
        UUID unknown = UUID.randomUUID();

        given(availabilityChecker.isFtpAvailable(any())).willReturn(true);
        given(ftpClient.downloadReports())
            .willReturn(singletonList(new Report(filePath, null)));
        given(parser.parse(any()))
            .willReturn(SampleData.parsedReport(filePath, asList(unknown, known), true));
        given(repo.findStatusById(known)).willReturn(Optional.of(LetterStatus.Uploaded));
        given(repo.findStatusById(unknown)).willReturn(Optional.empty());

        // when
        task.run();

        // then
        verify(repo).markAsPosted(eq(known), any());
    }

    @Test
    void should_delete_report_if_all_records_were_successfully_parsed() {
        final String reportName = "report.csv";
        final boolean allParsed = true;

        given(availabilityChecker.isFtpAvailable(any())).willReturn(true);

        given(ftpClient.downloadReports())
            .willReturn(singletonList(new Report(reportName, null)));

        given(parser.parse(any())).willReturn(SampleData.parsedReport(reportName, allParsed));

        given(repo.findStatusById(any())).willReturn(Optional.of(LetterStatus.Uploaded));

        // when
        task.run();

        // then
        verify(ftpClient).deleteReport(reportName);
    }

    @Test
    void should_not_delete_report_if_some_records_were_not_successfully_parsed() {
        final String reportName = "report.csv";
        final boolean allParsed = false;

        given(availabilityChecker.isFtpAvailable(any())).willReturn(true);

        given(ftpClient.downloadReports())
            .willReturn(singletonList(new Report(reportName, null)));

        given(parser.parse(any())).willReturn(SampleData.parsedReport(reportName, allParsed));

        given(repo.findStatusById(any())).willReturn(Optional.of(LetterStatus.Uploaded));

        // when
        task.run();

        // then
        verify(ftpClient, never()).deleteReport(anyString());
    }

    @Test
    void should_not_attempt_to_download_reports_during_ftp_downtime() {
        given(availabilityChecker.isFtpAvailable(any())).willReturn(false);

        // when
        task.run();

        // then
        verify(ftpClient, never()).downloadReports();
    }
}
