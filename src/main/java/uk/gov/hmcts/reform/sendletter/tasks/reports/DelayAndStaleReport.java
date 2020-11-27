package uk.gov.hmcts.reform.sendletter.tasks.reports;

import net.javacrumbs.shedlock.spring.annotation.SchedulerLock;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import uk.gov.hmcts.reform.sendletter.services.DelayedPrintService;
import uk.gov.hmcts.reform.sendletter.services.StaleLetterService;

import java.io.File;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

import static uk.gov.hmcts.reform.sendletter.util.TimeZones.EUROPE_LONDON;

@Component
@ConditionalOnBean(EmailSender.class)
@ConditionalOnProperty(prefix = "reports.delayed-stale-report", name = "enabled")
public class DelayAndStaleReport {
    private static final Logger log = LoggerFactory.getLogger(DelayAndStaleReport.class);

    private final DelayedPrintService deplayedPrintService;
    private final StaleLetterService staleLetterService;
    private final EmailSender emailSender;
    private final String[] recipients;
    private final int minStaleLetterAgeInBusinessDays;

    public static final String EMAIL_SUBJECT = "Send letter report for deplayed print and stale letters";
    public static final String ATTACHEMENT_DELAYED_PRINT_PREFIX = "Delayed-Print-Weekly-Report-";
    public static final String ATTACHEMENT_STALE_LETTER_PREFIX = "Stale-Letter-Weekly-Report-";
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyyMMdd");


    public DelayAndStaleReport(DelayedPrintService deplayedPrintService,
                               StaleLetterService staleLetterService,
                               EmailSender emailSender,
                               @Value("${reports.delayed-stale-report.recipients}") String[] recipients,
                               @Value("${stale-letters.min-age-in-business-days}") int minStaleLetterAgeInBusinessDays
    ) {
        this.deplayedPrintService = deplayedPrintService;
        this.staleLetterService = staleLetterService;
        this.emailSender = emailSender;
        this.recipients = recipients;
        this.minStaleLetterAgeInBusinessDays = minStaleLetterAgeInBusinessDays;
    }

    @SchedulerLock(name = "delayed-stale-report-summary", lockAtLeastFor = "PT5S")
    @Scheduled(cron = "${reports.delayed-stale-report.cron}", zone = EUROPE_LONDON)
    public void send() {
        if (recipients == null || recipients.length == 0) {
            log.error("No recipients configured to send delayed and stale letters");
        } else {
            LocalDateTime toDate = LocalDateTime.now();
            LocalDateTime fromDate = toDate.minusDays(6);
            try {
                File deplayLettersAttachment = deplayedPrintService.getDeplayLettersAttachment(fromDate, toDate,
                        minStaleLetterAgeInBusinessDays * 24);
                String deplayFileName = String.join("", ATTACHEMENT_DELAYED_PRINT_PREFIX,
                        toDate.format(FORMATTER), ".csv");

                Attachment attachment = new Attachment(deplayFileName, deplayLettersAttachment);

                emailSender.send(EMAIL_SUBJECT, recipients, attachment);

                log.info("Send email for {} ", EMAIL_SUBJECT);
            } catch (Exception e) {
                log.error("Error sending {}", EMAIL_SUBJECT, e);
            }
        }
    }

}
