package uk.gov.hmcts.reform.sendletter.services;

import lombok.RequiredArgsConstructor;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;
import uk.gov.hmcts.reform.sendletter.config.FtpConfigProperties;
import uk.gov.hmcts.reform.sendletter.entity.Letter;
import uk.gov.hmcts.reform.sendletter.entity.LetterRepository;
import uk.gov.hmcts.reform.sendletter.exception.FtpDownloadException;
import uk.gov.hmcts.reform.sendletter.exception.LetterFileNotFoundException;
import uk.gov.hmcts.reform.sendletter.exception.LetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.exception.TestingSupportLetterNotFoundException;
import uk.gov.hmcts.reform.sendletter.model.DownloadedLetterFile;
import uk.gov.hmcts.reform.sendletter.services.ftp.FtpClient;
import uk.gov.hmcts.reform.sendletter.services.ftp.ServiceFolderMapping;
import uk.gov.hmcts.reform.sendletter.services.util.FileNameHelper;
import uk.gov.hmcts.reform.sendletter.tasks.UploadLettersTask;

import java.util.UUID;

/**
 * Service for testing support operations.
 */
@Service
@ConditionalOnProperty(prefix = "testing-support", name = "enabled", havingValue = "true")
@RequiredArgsConstructor
public class TestingSupportService {

    private final LetterRepository letterRepository;
    private final ServiceFolderMapping serviceFolderMapping;
    private final FtpClient ftpClient;
    private final FtpConfigProperties ftpConfigProperties;

    /**
     * Downloads a letter file from SFTP.
     *
     * @param id The letter ID
     * @return The downloaded letter file
     */
    public DownloadedLetterFile downloadLetterFile(UUID id) {
        Letter letter = letterRepository
            .findById(id)
            .orElseThrow(() -> new TestingSupportLetterNotFoundException(id));

        String filename = FileNameHelper.generateName(letter);
        String path = getSftpPath(letter, filename);

        try {
            return new DownloadedLetterFile(filename, ftpClient.downloadFile(path));
        } catch (LetterFileNotFoundException exception) {
            throw new LetterFileNotFoundException(id, filename);
        } catch (RuntimeException exception) {
            throw new FtpDownloadException("Unable to download file '" + filename + "' from SFTP", exception);
        }
    }


    /**
     * Builds the SFTP path for an uploaded letter file.
     *
     * @param letter The letter
     * @param filename The generated letter filename
     * @return The SFTP path for the uploaded letter file
     */
    private String getSftpPath(Letter letter, String filename) {
        if (UploadLettersTask.SMOKE_TEST_LETTER_TYPE.equals(letter.getType())) {
            return String.join("/", ftpConfigProperties.getSmokeTestTargetFolder(), filename);
        }

        String serviceFolder = serviceFolderMapping
            .getFolderFor(letter.getService())
            .orElseThrow(() -> new LetterNotFoundException(letter.getId()));

        if (letter.getAdditionalData() != null
            && letter.getAdditionalData().has("isInternational")
            && letter.getAdditionalData().get("isInternational").asBoolean()) {
            serviceFolder = serviceFolder + UploadLettersTask.INTERNATIONAL_FOLDER;
        }

        return String.join("/", ftpConfigProperties.getTargetFolder(), serviceFolder, filename);
    }
}
