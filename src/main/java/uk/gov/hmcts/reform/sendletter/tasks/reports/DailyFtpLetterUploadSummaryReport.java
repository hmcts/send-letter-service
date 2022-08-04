package uk.gov.hmcts.reform.sendletter.tasks.reports;

import net.javacrumbs.shedlock.spring.annotation.SchedulerLock;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.services.FtpFileSummaryService;

import java.io.File;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static uk.gov.hmcts.reform.sendletter.util.TimeZones.EUROPE_LONDON;

@Component
@ConditionalOnBean(EmailSender.class)
@ConditionalOnProperty(prefix = "reports.ftp-uploaded-letters-summary", name = "enabled")
public class DailyFtpLetterUploadSummaryReport {
    private static final Logger log = LoggerFactory.getLogger(DailyFtpLetterUploadSummaryReport.class);

    private final FtpFileSummaryService ftpFileSummaryService;
    private final EmailSender emailSender;
    private final String[] recipients;

    public static final String EMAIL_SUBJECT = "FTP Uploaded Letters Summary split by services";
    public static final String ATTACHMENT_NAME_FORMAT = "Bulk-Print-FTP-Letters-Daily-Report-%s-%s.csv";
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");


    public DailyFtpLetterUploadSummaryReport(
        FtpFileSummaryService ftpFileSummaryService,
        EmailSender emailSender,
        @Value("${reports.upload-summary.recipients}") String[] recipients
    ) {
        this.ftpFileSummaryService = ftpFileSummaryService;
        this.emailSender = emailSender;
        this.recipients = recipients;
    }

    @SchedulerLock(name = "daily-ftp-uploaded-letters-summary", lockAtLeastFor = "PT5S")
    @Scheduled(cron = "${reports.ftp-uploaded-letters-summary.cron}", zone = EUROPE_LONDON)
    public void send() {
        if (recipients == null || recipients.length == 0) {
            log.error("No recipients configured to send daily FTP letters report");
        } else {
            Map<String, File> csvFiles = ftpFileSummaryService.getFtpFileUploadSummaryFiles();

            List<Attachment> attachments = getAttachments(csvFiles);

            if (!attachments.isEmpty()) {
                emailSender.send(EMAIL_SUBJECT, recipients, attachments.toArray(new Attachment[0]));
                log.info("Email sent for {} ", EMAIL_SUBJECT);
            } else {
                log.info("Not sending email for {} as there are no attachments", EMAIL_SUBJECT);
            }
        }
    }

    private List<Attachment> getAttachments(Map<String, File> csvFiles) {
        List<Attachment> attachments = new ArrayList<>();
        LocalDate today = LocalDate.now();

        for (Map.Entry<String, File> fileEntry : csvFiles.entrySet()) {
            Attachment attachment = new Attachment(
                String.format(ATTACHMENT_NAME_FORMAT, fileEntry.getKey(), today.format(FORMATTER)),
                fileEntry.getValue());
            attachments.add(attachment);
        }

        return attachments;
    }

}
