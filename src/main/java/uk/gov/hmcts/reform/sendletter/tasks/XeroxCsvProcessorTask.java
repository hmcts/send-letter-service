package uk.gov.hmcts.reform.sendletter.tasks;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.entity.LetterState;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.slc.model.LetterPrintStatus;
import uk.gov.hmcts.reform.slc.services.FtpAvailabilityChecker;
import uk.gov.hmcts.reform.slc.services.ReportParser;
import uk.gov.hmcts.reform.slc.services.steps.sftpupload.FtpClient;

import java.sql.Timestamp;
import java.util.UUID;

import static java.time.LocalTime.now;

public class XeroxCsvProcessorTask {
    private final LetterRepository repo;
    private final FtpClient ftpClient;
    private static final Logger logger = LoggerFactory.getLogger(XeroxCsvProcessorTask.class);
    private final FtpAvailabilityChecker ftpAvailabilityChecker;
    private final ReportParser parser;

    @Autowired
    public XeroxCsvProcessorTask(LetterRepository repo, FtpClient ftp,
                                 FtpAvailabilityChecker checker, ReportParser parser) {
        this.repo = repo;
        this.ftpClient = ftp;
        this.ftpAvailabilityChecker = checker;
        this.parser = parser;
    }

    public void run() {
        logger.trace("Running job");

        if (ftpAvailabilityChecker.isFtpAvailable(now())) {
            ftpClient
                .downloadReports()
                .stream()
                .map(parser::parse)
                .forEach(parsedReport -> {
                    parsedReport.statuses.forEach(this::updatePrintedAt);
                    ftpClient.deleteReport(parsedReport.path);
                });
        } else {
            logger.trace("FTP server not available, job cancelled");
        }
    }

    private void updatePrintedAt(LetterPrintStatus letterPrintStatus) {
        UUID id = UUID.fromString(letterPrintStatus.id);
        Letter letter = repo.findById(id).orElseThrow(() -> new LetterNotFoundException(id));
        letter.setSentToPrintAt(Timestamp.from(letterPrintStatus.printedAt.toInstant()));
        letter.setState(LetterState.Posted);
        repo.saveAndFlush(letter);
    }
}
