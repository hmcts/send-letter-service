package uk.gov.hmcts.reform.sendletter.util;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.gov.hmcts.reform.sendletter.entity.BasicLetterInfo;
import uk.gov.hmcts.reform.sendletter.model.out.LettersCountSummary;
import uk.gov.hmcts.reform.sendletter.services.ftp.FileInfo;
import uk.gov.hmcts.reform.sendletter.services.util.FileNameHelper;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import static java.lang.String.format;

public final class CsvWriter {
    private static final Logger logger = LoggerFactory.getLogger(CsvWriter.class);

    private static final FileAttribute<Set<PosixFilePermission>> ATTRIBUTE = PosixFilePermissions
        .asFileAttribute(PosixFilePermissions.fromString("rwx------"));

    private static final String[] LETTERS_COUNT_SUMMARY_CSV_HEADERS = {
        "Service", "Letters Uploaded"
    };

    private static final String[] STALE_LETTERS_CSV_HEADERS = {
        "Id", "Status", "Service", "CreatedAt", "SentToPrintAt"
    };

    private static final String[] DELAYED_LETTERS_EMAIL_CSV_HEADERS = {
        "FileName", "ServiceName", "ReceivedDate", "UploadedDate", "PrintedDate"
    };

    private static final String[] STALE_LETTERS_EMAIL_CSV_HEADERS = {
        "FileName", "ServiceName", "ReceivedDate", "UploadedDate"
    };

    private static final String[] FTP_UPLOADED_LETTERS_EMAIL_CSV_HEADERS = {
        "FileName", "UploadedAt"
    };

    private CsvWriter() {
        // utility class constructor
    }

    public static File writeLettersCountSummaryToCsv(
        List<LettersCountSummary> lettersCountSummary
    ) throws IOException {
        var path = Files.createTempFile("Letters-count-summary-", ".csv", ATTRIBUTE);// Compliant
        var csvFile = path.toFile();
        CSVFormat csvFileHeader = CSVFormat.DEFAULT.builder().setHeader(LETTERS_COUNT_SUMMARY_CSV_HEADERS).build();

        try (var fileWriter = new FileWriter(csvFile);
             var printer = new CSVPrinter(fileWriter, csvFileHeader)) {
            for (LettersCountSummary summary : lettersCountSummary) {
                printer.printRecord(summary.serviceName, summary.uploaded);
            }
        }
        return csvFile;
    }

    public static File writeStaleLettersToCsv(List<BasicLetterInfo> staleLetters) throws IOException {
        var path = Files.createTempFile("Stale-letters-", ".csv", ATTRIBUTE);// Compliant
        var csvFile = path.toFile();
        CSVFormat csvFileHeader = CSVFormat.DEFAULT.builder().setHeader(STALE_LETTERS_CSV_HEADERS).build();

        try (var fileWriter = new FileWriter(csvFile);
            var printer = new CSVPrinter(fileWriter, csvFileHeader)) {
            for (BasicLetterInfo staleLetter : staleLetters) {
                printer.printRecord(staleLetter.getId(), staleLetter.getStatus(),
                        staleLetter.getService(), staleLetter.getCreatedAt(), staleLetter.getSentToPrintAt());
            }
        }

        return csvFile;
    }

    public static File writeDelayedPostedLettersToCsv(Stream<BasicLetterInfo> letters) throws IOException {
        var path = Files.createTempFile("Delayed-letters-", ".csv", ATTRIBUTE);// Compliant
        var csvFile = path.toFile();
        CSVFormat csvFileHeader = CSVFormat.DEFAULT.builder().setHeader(DELAYED_LETTERS_EMAIL_CSV_HEADERS).build();
        var count = new AtomicInteger(0);

        try (var fileWriter = new FileWriter(csvFile);
             var printer = new CSVPrinter(fileWriter, csvFileHeader)) {
            letters.forEach(letter -> printDelayRecords(letter, printer, count));
        }

        logger.info("Number of weekly delayed print letters {}", count.get());
        return csvFile;
    }

    public static File writeStaleLettersReport(Stream<BasicLetterInfo> letters) throws IOException {
        var path = Files.createTempFile("Stale-letters-", ".csv", ATTRIBUTE);// Compliant
        var csvFile = path.toFile();
        CSVFormat csvFileHeader = CSVFormat.DEFAULT.builder().setHeader(STALE_LETTERS_EMAIL_CSV_HEADERS).build();
        var count = new AtomicInteger(0);

        try (var fileWriter = new FileWriter(csvFile);
             var printer = new CSVPrinter(fileWriter, csvFileHeader)) {
            letters.forEach(letter -> printStaleRecords(letter, printer, count));
        }

        logger.info("Number of weekly stale letters {}", count.get());
        return csvFile;
    }

    public static Map<String, File> writeFtpLettersToCsvFiles(
        Map<String, List<FileInfo>> serviceToLetters
    ) throws IOException {
        Map<String, File> csvFilesForServices = new HashMap<>();

        for (Map.Entry<String, List<FileInfo>> entry : serviceToLetters.entrySet()) {
            var path = Files.createTempFile(
                format("FTP-uploaded-letters-%s", entry.getKey()), ".csv", ATTRIBUTE);// Compliant

            var csvFile = path.toFile();
            CSVFormat csvFileHeader
                = CSVFormat.DEFAULT.builder().setHeader(FTP_UPLOADED_LETTERS_EMAIL_CSV_HEADERS).build();
            var count = new AtomicInteger(0);

            try (var fileWriter = new FileWriter(csvFile);
                 var printer = new CSVPrinter(fileWriter, csvFileHeader)) {
                entry.getValue().forEach(letter -> printFtpUploadedFile(letter, printer, count));
            }

            logger.info("Number of letters uploaded to FTP server for service {} {}", entry.getKey(), count.get());
            csvFilesForServices.put(entry.getKey(), csvFile);
        }

        return csvFilesForServices;
    }

    private static void printFtpUploadedFile(FileInfo fileInfo, CSVPrinter printer, AtomicInteger count) {
        try {
            printer.printRecord(fileInfo.path, fileInfo.modifiedAt);
            count.incrementAndGet();
        } catch (Exception e) {
            logger.error("Exception writing FTP uploaded file information ", e);
        }
    }

    private static void printStaleRecords(BasicLetterInfo letter, CSVPrinter printer, AtomicInteger count) {
        try {
            printer.printRecord(FileNameHelper.generateName(letter.getType(),
                letter.getService(), letter.getCreatedAt(), letter.getId(), true),
                    letter.getService(), letter.getCreatedAt(),
                    letter.getSentToPrintAt());
            count.incrementAndGet();
        } catch (Exception e) {
            logger.error("Stale letter exception ", e);
        }
    }

    private static void printDelayRecords(BasicLetterInfo letter, CSVPrinter printer, AtomicInteger count) {
        try {
            printer.printRecord(FileNameHelper.generateName(letter.getType(),
                letter.getService(), letter.getCreatedAt(), letter.getId(), true),
                    letter.getService(), letter.getCreatedAt(),
                    letter.getSentToPrintAt(), letter.getPrintedAt());
            count.incrementAndGet();
        } catch (Exception e) {
            logger.error("Delay letter posted exception ", e);
        }
    }
}
