package uk.gov.hmcts.reform.sendletter.services;

import jakarta.persistence.EntityManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import uk.gov.hmcts.reform.sendletter.SampleData;
import uk.gov.hmcts.reform.sendletter.config.ReportsServiceConfig;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;
import uk.gov.hmcts.reform.sendletter.entity.ReportRepository;
import uk.gov.hmcts.reform.sendletter.helper.FtpHelper;
import uk.gov.hmcts.reform.sendletter.logging.AppInsights;
import uk.gov.hmcts.reform.sendletter.model.ParsedReport;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpAvailabilityChecker;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpClient;
import uk.gov.hmcts.reform.sendletter.util.CsvReportWriter;

import java.time.LocalTime;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@DataJpaTest
class MarkLettersPostedServiceTest {

    @Autowired
    LetterRepository repository;

    @Autowired
    LetterService letterService;

    @Autowired
    ReportRepository reportRepository;

    @Autowired
    ReportsServiceConfig reportsServiceConfig;

    private LetterDataAccessService dataAccessService;

    @Autowired
    private EntityManager entityManager;

    ReportParser parser = new ReportParser();
    FtpAvailabilityChecker checker = mock(FtpAvailabilityChecker.class);
    AppInsights insights = mock(AppInsights.class);

    @BeforeEach
    void setUp() {
        dataAccessService = new LetterDataAccessService(repository);
    }

    @Test
    void marks_uploaded_letters_as_posted() throws Exception {
        // Create a letter in the Uploaded state.
        Letter letter = SampleData.letterEntity("bulkprint");
        letter.setStatus(LetterStatus.Uploaded);
        repository.saveAndFlush(letter);

        when(checker.isFtpAvailable(any(LocalTime.class))).thenReturn(true);
        try (LocalSftpServer server = LocalSftpServer.create()) {
            FtpClient client = FtpHelper.getSuccessfulClient(LocalSftpServer.port);
            MarkLettersPostedService task = new MarkLettersPostedService(
                dataAccessService,
                letterService,
                client,
                checker,
                parser,
                insights,
                reportsServiceConfig,
                reportRepository
            );

            // Prepare the CSV report and run the task.
            CsvReportWriter.writeReport(Stream.of(letter.getId()), server.reportFolder);
            task.processReports();
        }

        // Check that the letter has moved to the Posted state.
        entityManager.flush();
        letter = repository.findById(letter.getId()).get();
        assertThat(letter.getStatus()).isEqualTo(LetterStatus.Posted);
        // Check that printed at date has been set.
        assertThat(letter.getPrintedAt()).isNotNull();
        assertThat(letter.getFileContent()).isNull();

        verify(insights).trackPrintReportReceived(any(ParsedReport.class));
    }
}
