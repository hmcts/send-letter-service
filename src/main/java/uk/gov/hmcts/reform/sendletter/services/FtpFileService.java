package uk.gov.hmcts.reform.sendletter.services;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.services.ftp.FileInfo;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpClient;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;

import java.time.LocalTime;
import java.time.ZoneId;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static uk.gov.hmcts.reform.sendletter.util.TimeZones.EUROPE_LONDON;

@Service
public class FtpFileService {
    private static final Logger log = LoggerFactory.getLogger(FtpFileService.class);

    private final FtpClient ftp;
    private final ServiceFolderMapping serviceFolderMapping;

    // region constructor
    public FtpFileService(FtpClient ftp,
                          ServiceFolderMapping serviceFolderMapping) {
        this.ftp = ftp;
        this.serviceFolderMapping = serviceFolderMapping;
    }
    // endregion

    public Map<String, List<FileInfo>> listLettersForAllServices() {
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
                } catch (Exception e) {
                    log.error("Error listing files from FTP folder {}", folder, e);
                }
            });

        log.info("Completed listing uploaded letters to FTP server at {}", LocalTime.now(ZoneId.of(EUROPE_LONDON)));
        return ftpUploadedLetters;
    }

}
