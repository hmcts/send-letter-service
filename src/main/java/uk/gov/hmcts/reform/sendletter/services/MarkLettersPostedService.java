package uk.gov.hmcts.reform.sendletter.services;


import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.config.ReportsServiceConfig;
import uk.gov.hmcts.reform.sendletter.entity.LetterStatus;
import uk.gov.hmcts.reform.sendletter.entity.Report;
import uk.gov.hmcts.reform.sendletter.entity.ReportRepository;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.logging.AppInsights;
import uk.gov.hmcts.reform.sendletter.model.LetterPrintStatus;
import uk.gov.hmcts.reform.sendletter.model.ParsedReport;
import uk.gov.hmcts.reform.sendletter.model.out.PostedReportTaskResponse;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpClient;
import uk.gov.hmcts.reform.sendletter.services.ftp.IFtpAvailabilityChecker;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.DateTimeParseException;
import java.time.format.SignStyle;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.time.temporal.ChronoField.DAY_OF_MONTH;
import static java.time.temporal.ChronoField.MONTH_OF_YEAR;
import static java.time.temporal.ChronoField.YEAR;
import static uk.gov.hmcts.reform.sendletter.util.TimeZones.EUROPE_LONDON;

/**
 * Fetches reports from SFTP concerning posted
 * letters and sets status as Posted in the database.
 */
@Service
@RequiredArgsConstructor
public class MarkLettersPostedService {

    private final LetterDataAccessService dataAccessService;
    private final LetterService letterService;
    private final FtpClient ftpClient;
    private final IFtpAvailabilityChecker ftpAvailabilityChecker;
    private final ReportParser parser;
    private final AppInsights insights;
    private final ReportsServiceConfig reportsServiceConfig;

    private static final Logger logger = LoggerFactory.getLogger(MarkLettersPostedService.class);
    private static final String TASK_NAME = "MarkLettersPosted";
    private static final Pattern DATE_PATTERN = Pattern.compile("(\\d{4}|\\d{2})-\\d{2}-(\\d{4}|\\d{2})");
    private static final Pattern REPORT_CODE_PATTERN = Pattern.compile("(?<=MOJ_)[^_.]+");
    private final ReportRepository reportRepository;

    // default formatter used when resolving dates from a report path
    public static final DateTimeFormatter REPORT_DATE_FORMATTER;

    static {
        REPORT_DATE_FORMATTER = new DateTimeFormatterBuilder()
            .appendValue(DAY_OF_MONTH, 2)
            .appendLiteral('-')
            .appendValue(MONTH_OF_YEAR, 2)
            .appendLiteral('-')
            .appendValue(YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
            .toFormatter();
    }

    /**
     * Fetches reports from SFTP and sets status as Posted in the database.
     */
    public List<PostedReportTaskResponse> processReports() {
        if (!ftpAvailabilityChecker.isFtpAvailable(LocalTime.now(ZoneId.of(EUROPE_LONDON)))) {
            logger.info("Not processing '{}' task due to FTP downtime window", TASK_NAME);
            return Collections.emptyList();
        }

        logger.info("Started '{}' task", TASK_NAME);
        final AtomicReference<PostedReportTaskResponse> currentResponse = new AtomicReference<>();
        final List<PostedReportTaskResponse> responseList = new ArrayList<>();
        try {
            ftpClient
                .downloadReports()
                .stream()
                .map(parser::parse)
                .forEach(parsedReport -> {
                    insights.trackPrintReportReceived(parsedReport);
                    logger.info(
                        "Updating letters from report {}. Letter count: {}",
                        parsedReport.path,
                        parsedReport.statuses.size()
                    );

                    Optional<String> reportCode = extractReportCodeFromParsedReport(parsedReport);

                    if (reportCode.isEmpty()) {
                        // this is an edge case where the report filename didn't contain a know reportCode
                        // and there were no letters referenced in the parsed report that could be used
                        // to determine a report code from their assigned service.
                        //
                        // When this happens, processing is allowed to move on to the next parsed report,
                        // but an error response will be added to indicate that a report couldn't be married
                        // up to a specific service.
                        currentResponse.set(new PostedReportTaskResponse("UNKNOWN", parsedReport.reportDate));
                        currentResponse.get().markAsFailed(
                            String.format("Service not found for report with name '%s'", parsedReport.path));
                    } else {

                        LocalDate reportDate = extractDateFromReportPath(parsedReport.path)
                            .orElse(parsedReport.reportDate);

                        // create a response now so that we can ensure a response in the case of an
                        // exceptional failure during the markAsPosted iteration below.
                        currentResponse.set(new PostedReportTaskResponse(reportCode.get(), reportDate));

                        long count = parsedReport.statuses.stream()
                            .filter(status -> markAsPosted(status, parsedReport.path))
                            .count();

                        currentResponse.get().setMarkedPostedCount(count);

                        if (parsedReport.allRowsParsed) {
                            logger.info("Report {} successfully parsed, deleting", parsedReport.path);
                            ftpClient.deleteReport(parsedReport.path);
                            // now that we've processed the file, we can save a report.
                            reportRepository.save(Report.builder()
                                .reportName(parsedReport.path)
                                .reportDate(reportDate)
                                .service(reportCode.get())
                                .printedLettersCount(count)
                                .build());
                        } else {
                            logger.warn("Report {} contained invalid rows, file not removed.", parsedReport.path);
                            currentResponse.get().markAsFailed("Report " + parsedReport.path
                                + " contained invalid rows");
                        }
                    }
                    responseList.add(currentResponse.getAndSet(null));
                });

            logger.info("Completed '{}' task", TASK_NAME);
        } catch (Exception e) {
            logger.error("An error occurred when downloading reports from SFTP server", e);
            Optional.ofNullable(currentResponse.getAndSet(null)).ifPresent(ptr -> {
                ptr.markAsFailed("An error occurred when processing downloaded reports from the SFTP server: "
                    + e.getMessage());
                responseList.add(ptr);
            });
        }
        return responseList;
    }

    /**
     * Marks the letter as posted in the database.
     *
     * @param letterPrintStatus The letter print status
     * @param reportFileName    The report file name
     */
    private boolean markAsPosted(LetterPrintStatus letterPrintStatus, String reportFileName) {
        final AtomicBoolean markedAsPosted = new AtomicBoolean(false);
        dataAccessService
            .findLetterStatus(letterPrintStatus.id)
            .ifPresentOrElse(
                status -> {
                    if (status.equals(LetterStatus.Uploaded)) {
                        dataAccessService.markLetterAsPosted(
                            letterPrintStatus.id,
                            letterPrintStatus.printedAt.toLocalDateTime()
                        );
                        markedAsPosted.set(true);
                        logger.info("Marked letter {} as posted", letterPrintStatus.id);
                    } else {
                        logger.warn(
                            "Failed to mark letter {} as posted - unexpected status: {}. Report file name: {}",
                            letterPrintStatus.id,
                            status,
                            reportFileName
                        );
                    }
                },
                () -> logger.error(
                    "Failed to mark letter {} as posted - unknown letter. Report file name: {}",
                    letterPrintStatus.id,
                    reportFileName
                )
            );
        return markedAsPosted.get();
    }

    private Optional<String> extractReportCodeFromParsedReport(ParsedReport parsedReport) {
        // the ideal is that we can simply extract the report code from the
        // report filename, though we do need to ensure that extracted report
        // name is known/expected.
        Matcher matcher = REPORT_CODE_PATTERN.matcher(parsedReport.path);
        if (matcher.find()) {
            String reportCode = matcher.group();
            if (reportsServiceConfig.getReportCodes().contains(reportCode)) {
                return Optional.of(reportCode);
            }
        }

        logger.info("Could not determine report code from report filename: {}", parsedReport.path);

        // if the above doesn't provide a result, start digging through the entries in
        // the report file until we can find a service that we can map to a report code
        for (LetterPrintStatus lps : parsedReport.statuses) {
            // initially, we just need to find a letter that exists
            try {
                Optional<String> service = dataAccessService.findLetterService(lps.id);
                if (service.isPresent()) {
                    // then use that letter, and potentially it's status to look up the right code
                    uk.gov.hmcts.reform.sendletter.model.out.LetterStatus status =
                        letterService.getStatus(lps.id, Boolean.TRUE.toString(), Boolean.FALSE.toString());
                    String code = reportsServiceConfig.getReportCode(service.get(), status);
                    if (code != null) {
                        return Optional.of(code);
                    }
                }
            } catch (LetterNotFoundException e) {
                logger.warn("Letter not found for id '{}' during report code lookup", lps.id);
            }
        }
        return Optional.empty();
    }

    private Optional<LocalDate> extractDateFromReportPath(String path) {
        Matcher matcher = DATE_PATTERN.matcher(path);
        if (matcher.find()) {
            String dateString = matcher.group();
            try {
                return Optional.of(LocalDate.parse(dateString,
                    dateString.charAt(4) == '-' ? DateTimeFormatter.ISO_LOCAL_DATE : REPORT_DATE_FORMATTER));
            } catch (DateTimeParseException e) {
                logger.warn("Could not parse date from report path '{}'", path);
            }
        }
        return Optional.empty();
    }
}
