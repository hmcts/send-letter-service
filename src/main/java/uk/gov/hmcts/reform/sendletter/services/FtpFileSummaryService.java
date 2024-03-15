package uk.gov.hmcts.reform.sendletter.services;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.services.ftp.FileInfo;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpClient;
import uk.gov.hmcts.reform.sendletter.services.ftp.IFtpAvailabilityChecker;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;
import uk.gov.hmcts.reform.sendletter.util.CsvWriter;

import java.io.File;
import java.io.IOException;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.util.Collections.emptyMap;
import static uk.gov.hmcts.reform.sendletter.util.TimeZones.EUROPE_LONDON;

/**
 * Service for FTP file summary data.
 */
@Service
public class FtpFileSummaryService {
    private static final Logger log = LoggerFactory.getLogger(FtpFileSummaryService.class);

    private final IFtpAvailabilityChecker ftpAvailabilityChecker;
    private final FtpClient ftp;
    private final ServiceFolderMapping serviceFolderMapping;

    /**
     * Constructor for the FtpFileSummaryService.
     * @param ftpAvailabilityChecker The FTP availability checker
     * @param ftp The FTP client
     * @param serviceFolderMapping The service folder mapping
     */
    public FtpFileSummaryService(IFtpAvailabilityChecker ftpAvailabilityChecker,
                                 FtpClient ftp,
                                 ServiceFolderMapping serviceFolderMapping) {
        this.ftpAvailabilityChecker = ftpAvailabilityChecker;
        this.ftp = ftp;
        this.serviceFolderMapping = serviceFolderMapping;
    }

    /**
     * Get the FTP file upload summary files.
     * @return The FTP file upload summary files
     */
    public Map<String, File> getFtpFileUploadSummaryFiles() {
        if (!ftpAvailabilityChecker.isFtpAvailable(LocalTime.now(ZoneId.of(EUROPE_LONDON)))) {
            log.info("Not generating Bulk Print FTP Uploaded Letters Summary due to FTP downtime window");
            return emptyMap();
        }

        try {
            Map<String, List<FileInfo>> ftpLettersForServices = listLettersForAllServices();
            return CsvWriter.writeFtpLettersToCsvFiles(ftpLettersForServices);
        } catch (IOException exception) {
            log.error("Unable to generate report", exception);
        }
        return emptyMap();
    }

    /**
     * List letters for all services.
     * @return The letters for all services
     */
    private Map<String, List<FileInfo>> listLettersForAllServices() {
        Map<String, List<FileInfo>> ftpUploadedLetters = new HashMap<>();
        log.info("Started listing uploaded letters to FTP server at {}", LocalTime.now(ZoneId.of(EUROPE_LONDON)));
        serviceFolderMapping
            .getFolders()
            .forEach(folder -> {
                try {
                    // new connection per folder
                    ftp.runWith(sftpClient -> {
                        List<FileInfo> filesUploaded = ftp.listLetters(folder, sftpClient);
                        ftpUploadedLetters.put(folder, filesUploaded);

                        return null;
                    });
                } catch (Exception exception) {
                    log.error("Error listing files from FTP folder {}", folder, exception);
                }
            });

        log.info("Completed listing uploaded letters to FTP server at {}", LocalTime.now(ZoneId.of(EUROPE_LONDON)));
        return ftpUploadedLetters;
    }

}
